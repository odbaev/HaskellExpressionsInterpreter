using System;
using System.IO;
using System.Diagnostics;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Linq;


namespace HaskellExpressionsInterpreter
{
    public class InterpreterEventArgs : EventArgs
    {
        public string Result { get; }

        public InterpreterEventArgs(string result)
        {
            Result = result;
        }
    }

    public class Interpreter
    {
        private Process ghci;

        public delegate void InterpreterEventHandler(object sender, InterpreterEventArgs e);

        public event InterpreterEventHandler ErrorReceived;
        public event InterpreterEventHandler OutputReceived;

        private string error;
        private List<string> output = new List<string>();

        public bool IsBusy { get; private set; }

        private string initExpr;
        private int step;
        private bool needAllSteps;

        private enum State { Start, ApReflect, SimpleReflect, Initial };

        private State state = State.Start;

        private readonly Dictionary<string, string> ApReflectOperations = new Dictionary<string, string>
        {
            {"pure", "pure''"},
            {"fmap", "fmap''"},
            {"sequenceA", "sequenceA'"},
            {"traverse", "traverse'" },
            {"<$>", "-$-"},
            {"<*>", "-*-"},
            {"(+)", "(.+)"},
            {"(-)", "(.-)"},
            {"(*)", "(.*)"},
            {"(/)", "(./)"},
            {"(^)", "(.^)"},
            {"(^^)", "(.^^)"},
            {"(**)", "(.**)"},
            {"(<)", "(.<)"},
            {"(>)", "(.>)"},
            {"(<=)", "(.<=)"},
            {"(>=)", "(.>=)"},
            {"(==)", "(.==)"},
            {"(/=)", "(./=)"},
            {"(&&)", "(.&&)"},
            {"(||)", "(.||)"},
            {"(++)", "(.++)"},
            {"(:)", "(.:)"},
            {"(!!)", "(.!!)"}
        };

        public Interpreter(string dir)
        {
            ghci = new Process();
            ghci.StartInfo.FileName = "ghci";
            ghci.StartInfo.WorkingDirectory = dir;
            ghci.StartInfo.UseShellExecute = false;
            ghci.StartInfo.CreateNoWindow = true;
            ghci.StartInfo.RedirectStandardInput = true;
            ghci.StartInfo.RedirectStandardOutput = true;
            ghci.StartInfo.RedirectStandardError = true;

            ghci.OutputDataReceived += Ghci_OutputDataReceived;
            ghci.ErrorDataReceived += Ghci_ErrorDataReceived;

            ghci.Start();

            ghci.BeginOutputReadLine();
            ghci.BeginErrorReadLine();

            StreamWriter sw = ghci.StandardInput;

            sw.WriteLine(":set prompt \"\"");
            sw.WriteLine(":set -package simple-reflect");
            sw.WriteLine(":set -package ap-reflect");
            sw.WriteLine(":l Extensions.hs");
        }

        public void Close()
        {
            ghci.StandardInput.WriteLine(":q");
            ghci.WaitForExit();
            ghci.Close();
        }

        private void Ghci_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            if (state == State.Start) return;

            if (e.Data != string.Empty)
            {
                output.Add(e.Data);
                return;
            }

            if (error != string.Empty)
            {
                switch (state)
                {
                    case State.ApReflect:
                        state = State.SimpleReflect;
                        Interpret();
                        break;
                    case State.SimpleReflect:
                        state = State.Initial;
                        Interpret();
                        break;
                    case State.Initial:
                        if (ErrorReceived != null)
                        {
                            error = Regex.Replace(error, @"<interactive>:\d+:", "Position ");
                            ErrorReceived(this, new InterpreterEventArgs(error));
                        }
                        IsBusy = false;
                        break;
                }
                return;
            }

            if (OutputReceived != null)
            {
                string res = needAllSteps ? string.Join(Environment.NewLine, output) : output[0]; 
                OutputReceived(this, new InterpreterEventArgs(res));
            }

            IsBusy = false;
        }

        private void Ghci_ErrorDataReceived(object sender, DataReceivedEventArgs e)
        {
            error += e.Data + Environment.NewLine;
        }

        private string GetApReflectOp(Match m)
        {
            if (m.Value[0] == '"')
            {
                return m.Value;
            }

            string op = m.Value[0] == '(' ? m.Value.Replace(" ", "") : m.Value;

            return ApReflectOperations[op];
        }

        private string MakeApReflectExpression()
        {
            const string pattern = @""".*?""|pure|fmap|sequenceA|traverse|<[\$\*]>|\(\s*([\+\-\*/:\^<>]|\+{2}|\^{2}|\*{2}|!!|<=|>=|==|/=|&&|\|{2})\s*\)";

            return Regex.Replace(initExpr, pattern, new MatchEvaluator(GetApReflectOp));
        }

        private void Interpret()
        {
            output.Clear();
            error = "";

            StreamWriter sw = ghci.StandardInput;

            switch (state)
            {
                case State.ApReflect:
                    if (!Regex.IsMatch(initExpr, @"pure|fmap|<[\$\*]>|sequenceA|traverse"))
                    {
                        state = State.SimpleReflect;
                        Interpret();
                        return;
                    }

                    string expr = MakeApReflectExpression();
                    sw.WriteLine($"mapM_ print . reductions $ {expr}");        
                    break;

                case State.SimpleReflect:
                    sw.WriteLine($"mapM_ print . reduction $ {initExpr}");
                    break;

                case State.Initial:
                    sw.WriteLine(initExpr);
                    break;
            }
            sw.WriteLine("putStrLn \"\"");
        }

        public void ShowNextStep(string expression)
        {
            IsBusy = true;

            if (initExpr != expression)
            {
                needAllSteps = false;

                initExpr = expression;

                state = State.ApReflect;
                Interpret();

                step = 1;
                return;
            }

            if (error != String.Empty || step == output.Count || needAllSteps)
            {
                IsBusy = false;
                return;
            }

            if (OutputReceived != null)
            {
                OutputReceived(this, new InterpreterEventArgs(output[step++]));
            }

            IsBusy = false;
        }

        public void ShowAllSteps(string expression)
        {
            IsBusy = true;            

            if (initExpr != expression)
            {
                needAllSteps = true;

                initExpr = expression;

                state = State.ApReflect;
                Interpret();

                return;
            }

            if (error != String.Empty || step == output.Count || needAllSteps)
            {
                IsBusy = false;
                return;
            }

            if (OutputReceived != null)
            {
                string res = string.Join(Environment.NewLine, output.Skip(step));              
                OutputReceived(this, new InterpreterEventArgs(res));

                step = output.Count;
            }

            IsBusy = false;
        }
    }
}
