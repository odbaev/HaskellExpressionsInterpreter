using System;
using System.IO;
using System.Diagnostics;
using System.Collections.Generic;
using System.Text.RegularExpressions;


namespace HaskellExpressionsInterpreter
{
    public class Interpreter
    {
        private Process ghc;

        private string tempDir;
        private string initExpr;
        private string error;
        private List<string> output;
        private int step;

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
            tempDir = dir;

            ghc = new Process();
            ghc.StartInfo.FileName = "runhaskell";
            ghc.StartInfo.WorkingDirectory = tempDir;
            ghc.StartInfo.UseShellExecute = false;
            ghc.StartInfo.CreateNoWindow = true;
            ghc.StartInfo.RedirectStandardOutput = true;
            ghc.StartInfo.RedirectStandardError = true;
            ghc.OutputDataReceived += (s, e) => { if (!String.IsNullOrEmpty(e.Data)) output.Add(e.Data); };
        }

        private void MakeErrorMessage()
        {
            string err = "";

            foreach (string str in error.Split(new string[] { "Initial.hs:" }, StringSplitOptions.None))
            {
                if (str.StartsWith("8:"))
                {
                    int endPos = str.IndexOfAny(new char[] { ':', '\r', '\n' }, 3);
                    int pos = Int32.Parse(str.Substring(2, endPos - 2)) - 15;

                    if (pos > 0)
                    {
                        string errMsg = str.Substring(endPos).Replace("In the expression: print $", "In the expression:");

                        int eqPos = errMsg.IndexOf("In an equation for `main':");
                        if (eqPos != -1)
                        {
                            errMsg = errMsg.Remove(eqPos);
                        }

                        err += "Position " + pos.ToString() + errMsg;
                    }
                }
            }

            error = err != String.Empty ? err.Replace("\r\n", "\n") : "Error";
        }

        private void RunHaskell(string expr, string outputFile)
        {
            string filePath = Path.Combine(tempDir, outputFile);
            File.Copy(Path.Combine(tempDir, "Template" + outputFile), filePath, true);

            File.AppendAllText(filePath, expr);

            ghc.StartInfo.Arguments = outputFile;
            ghc.Start();

            error = ghc.StandardError.ReadToEnd();
            output = new List<string>();
            ghc.BeginOutputReadLine(); 

            ghc.WaitForExit();
            ghc.CancelOutputRead();
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
            if (Regex.IsMatch(initExpr, @"pure|fmap|<[\$\*]>|sequenceA|traverse"))
            {
                const string apReflectFile = "ApReflect";
                string expr = MakeApReflectExpression();

                RunHaskell(expr, apReflectFile + ".hs");

                if (error == String.Empty) return;
            }

            const string simpleReflectFile = "SimpleReflect";

            RunHaskell(initExpr, simpleReflectFile + ".hs");
            if (error == String.Empty) return;

            const string initFile = "Initial";

            RunHaskell(initExpr, initFile + ".hs");

            if (error != String.Empty)
            {
                MakeErrorMessage();
            }
        }

        public string ShowAllSteps(string expression)
        {
            if (initExpr != expression)
            {
                initExpr = expression;
                Interpret();
            }

            if (error != String.Empty)
            {
                return error;
            }

            step = output.Count;

            return String.Join("\n", output);
        }

        public string ShowNextStep(string expression)
        {
            if (initExpr != expression)
            {
                initExpr = expression;
                Interpret();

                if (error != String.Empty)
                {
                    return error;
                }

                step = 0;
            }

            if (error != String.Empty || step == output.Count)
            {
                return "";
            }

            return output[step++];
        }
    }
}
