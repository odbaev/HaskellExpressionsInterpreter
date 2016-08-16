using System;
using System.Linq;
using System.Collections.Generic;
using System.Windows.Documents;
using System.Text.RegularExpressions;


namespace HaskellExpressionsInterpreter
{
    public static class SyntaxHighlighting
    {
        private const string pattern =
            @"(?<string>"".*?""|"".*|'.')|(?<applicative><[\$\*]>?)|" +
            @"(?<operation>\(\s*([\+\-\*/:\^<>\.\$]|\+{2}|\^{2}|\*{2}|!!|<=|>=|==|/=|&&|\|{2})\s*\)|[\+\-\*/:\^!<>=&\@|\\|\.|\$])|" +
            @"(?<!\w|_|')((?<number>\d+(\.\d+)?([eE][\+\-]?\d+)?)|(?<bool>False|True)|" +
            @"(?<functor>Just|Nothing|Left|Right|Const|Identity|ZipList)|" + 
            @"(?<keyword>if|then|else|let|in|where|case|of|do))(?!\w|_|')";

        private static readonly Dictionary<string, string> colors = new Dictionary<string, string>
        {
            {"operation", "#81D41C"},
            {"applicative","#E52B50"},
            {"functor", "#7B68EE"},
            {"number", "#FF8C69"},
            {"string", "#FBD151"},
            {"bool", "#5CCCCC"},
            {"keyword", "#8AC6F2"}
        };

        public static void Highlight(Run run)
        {
            TextRange range = new TextRange(run.ContentStart, run.ContentStart);
            TextPointer selectionStart, selectionEnd;

            foreach (Match m in Regex.Matches(run.Text, pattern).Cast<Match>().Reverse())
            {
                selectionStart = run.ContentStart.GetPositionAtOffset(m.Index);
                selectionEnd = run.ContentStart.GetPositionAtOffset(m.Index + m.Length);

                range.Select(selectionStart, selectionEnd);

                foreach (KeyValuePair<string, string> kvp in colors)
                {
                    if (m.Groups[kvp.Key].Success)
                    {
                        range.ApplyPropertyValue(Run.ForegroundProperty, kvp.Value);

                        break;
                    }
                }
            }
        }
    }
}
