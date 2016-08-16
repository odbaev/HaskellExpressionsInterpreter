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
        private Process prog;

        private string tempDir;
        private string initExpr;
        private string error;
        private string[] output;
        private int step;

        private readonly Dictionary<string, string> ApReflectOperations = new Dictionary<string, string>
        {
            {"pure", "pure''"},
            {"fmap", "fmap''"},
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
            ghc.StartInfo.FileName = "ghc";
            ghc.StartInfo.WorkingDirectory = tempDir;
            ghc.StartInfo.UseShellExecute = false;
            ghc.StartInfo.CreateNoWindow = true;
            ghc.StartInfo.RedirectStandardOutput = true;
            ghc.StartInfo.RedirectStandardError = true;

            prog = new Process();
            prog.StartInfo.WorkingDirectory = tempDir;
            prog.StartInfo.UseShellExecute = false;
            prog.StartInfo.CreateNoWindow = true;
            prog.StartInfo.RedirectStandardOutput = true;
            prog.StartInfo.RedirectStandardError = true;
        }

        private void MakeErrorMessage()
        {
            string err = "";

            foreach (string str in error.Split(new string[] { "Initial.hs:" }, StringSplitOptions.None))
            {
                if (str.StartsWith("20:"))
                {
                    int endPos = str.IndexOfAny(new char[] { ':', '\r', '\n' }, 4);
                    int pos = Int32.Parse(str.Substring(3, endPos - 3)) - 15;

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

        private bool IsCorrect(string expr, string outputFile)
        {
            string filePath = Path.Combine(tempDir, outputFile);
            File.Copy(Path.Combine(tempDir, "Template" + outputFile), filePath, true);

            File.AppendAllText(filePath, expr);

            ghc.StartInfo.Arguments = "--make " + outputFile;
            ghc.Start();

            error = ghc.StandardError.ReadToEnd();

            ghc.WaitForExit();

            return error == String.Empty;
        }

        private void GetOutput(string file)
        {
            prog.StartInfo.FileName = Path.Combine(tempDir, file);
            prog.Start();

            output = prog.StandardOutput.ReadToEnd().Split(new char[] { '\n', '\r' }, StringSplitOptions.RemoveEmptyEntries);

            prog.WaitForExit();
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
            const string pattern = @""".*?""|pure|fmap|<[\$\*]>|\(\s*([\+\-\*/:\^<>]|\+{2}|\^{2}|\*{2}|!!|<=|>=|==|/=|&&|\|{2})\s*\)";

            return Regex.Replace(initExpr, pattern, new MatchEvaluator(GetApReflectOp));
        }

        private void Interpret()
        {
            if (Regex.IsMatch(initExpr, @"pure|fmap|<[\$\*]>|sequenceA|traverse"))
            {
                const string apReflectFile = "ApReflect";
                string expr = MakeApReflectExpression();

                if (IsCorrect(expr, apReflectFile + ".hs"))
                {
                    GetOutput(apReflectFile);
                    return;
                }
            }

            const string simpleReflectFile = "SimpleReflect";

            if (IsCorrect(initExpr, simpleReflectFile + ".hs"))
            {
                GetOutput(simpleReflectFile);
                return;
            }

            const string initFile = "Initial";

            if (IsCorrect(initExpr, initFile + ".hs"))
            {
                GetOutput(initFile);
            }
            else
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

            step = output.Length;

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

            if (error != String.Empty || step == output.Length)
            {
                return "";
            }

            return output[step++];
        }
    }
}
