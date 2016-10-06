using System;
using System.IO;
using System.Windows;
using System.Windows.Input;
using System.Windows.Documents;
using System.Windows.Controls;
using System.Diagnostics;
using System.Threading.Tasks;


namespace HaskellExpressionsInterpreter
{
    /// <summary>
    /// Логика взаимодействия для MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private Interpreter hsInt;
        private string tempDir;
        private string curExpr;
        private string lastExpr;
        private int caretPos;

        #region window load

        public MainWindow()
        {
            InitializeComponent();

            Rect bounds = Properties.Settings.Default.WindowPosition;
            this.Width = bounds.Width;
            this.Height = bounds.Height;
            this.Left = bounds.Left;
            this.Top = bounds.Top;

            WindowState = Properties.Settings.Default.WindowState;

            CheckTools();
        }

        private bool AppExists(string app)
        {
            string pathes = Environment.GetEnvironmentVariable("PATH");

            foreach (string path in pathes.Split(';'))
            {
                if (File.Exists(Path.Combine(path, app)))
                {
                    return true;
                }
            }

            return false;
        }

        private void CheckTools()
        {
            if (!AppExists("ghc.exe") || !AppExists("cabal.exe"))
            {
                InstallWindow iw = new InstallWindow();
                iw.ShowDialog();

                ExprTextBox.IsReadOnly = true;
                return;
            }

            Process p = new Process();
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.RedirectStandardOutput = true;
            p.StartInfo.RedirectStandardError = true;

            p.StartInfo.FileName = "ghc-pkg";
            p.StartInfo.Arguments = "list";
            p.Start();

            string libs = p.StandardOutput.ReadToEnd();

            p.WaitForExit();

            bool apReflect = libs.IndexOf(" ap-reflect-") != -1;
            bool simpleReflect = libs.IndexOf(" simple-reflect-") != -1;

            if (apReflect && simpleReflect)
            {
                return;
            }

            LoadWindow lw = new LoadWindow();
            lw.Show();

            p.StartInfo.FileName = "cabal";
            p.StartInfo.Arguments = "update";
            p.Start();
            p.WaitForExit();

            if (!apReflect)
            {
                p.StartInfo.Arguments = "install ap-reflect-0.2";
                p.Start();
                p.WaitForExit();
            }

            if (!simpleReflect)
            {
                p.StartInfo.Arguments = "install simple-reflect";
                p.Start();
                p.WaitForExit();
            }

            lw.Close();
        }

        private void MainWindow_Loaded(object sender, RoutedEventArgs e)
        {
            tempDir = Path.Combine(Path.GetTempPath(), "HEI {" + Guid.NewGuid() + "}");
            Directory.CreateDirectory(tempDir);

            File.WriteAllBytes(Path.Combine(tempDir, "Extensions.hs"), Properties.Resources.Extensions);

            hsInt = new Interpreter(tempDir);
            hsInt.OutputReceived += HsInt_OutputReceived;
            hsInt.ErrorReceived += HsInt_ErrorReceived;

            ExprTextBox.Focus();
        }

        #endregion

        #region window events

        private void ExitCommand_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            this.Close();
        }

        private void MainWindow_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            Properties.Settings.Default.WindowPosition = this.RestoreBounds;

            if (WindowState != WindowState.Minimized)
            {
                Properties.Settings.Default.WindowState = WindowState;
            }

            Properties.Settings.Default.Save();
        }

        private void MainWindow_Closed(object sender, EventArgs e)
        {
            hsInt.Close();

            try
            {
                Directory.Delete(tempDir, true);
            }
            catch { }
        }

        private void MainWindow_StateChanged(object sender, EventArgs e)
        {
            ExprTextBox.Focus();
        }

        private void TitleBar_MouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            this.DragMove();
        }

        private void MinimizeButton_Click(object sender, RoutedEventArgs e)
        {
            WindowState = WindowState.Minimized;
        }

        private void MaximizeRestoreButton_Click(object sender, RoutedEventArgs e)
        {
            if (WindowState == WindowState.Normal)
            {
                WindowState = WindowState.Maximized;
            }
            else
            {
                WindowState = WindowState.Normal;
            }
        }

        #endregion

        #region textbox commands

        private void CopyCommand_CanExecute(object sender, CanExecuteRoutedEventArgs e)
        {
            e.CanExecute = !((RichTextBox)sender).Selection.IsEmpty;
        }

        private void CopyCommand_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            RichTextBox textBox = (RichTextBox)sender;

            textBox.Selection.ApplyPropertyValue(Run.BackgroundProperty, "#212121");
            textBox.Copy();
        }

        private void PasteCommand_CanExecute(object sender, CanExecuteRoutedEventArgs e)
        {
            e.CanExecute = Clipboard.ContainsText();
        }

        private void PasteCommand_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            string text = Clipboard.GetText();

            int newLineIndex = text.IndexOfAny(new char[] { '\r', '\n' });
            if (newLineIndex != -1)
            {
                text = text.Substring(0, newLineIndex);
            }

            Clipboard.SetText(text);

            ExprTextBox.Paste();
        }

        private void ClearButton_Click(object sender, RoutedEventArgs e)
        {
            ExprTextBox.Document.Blocks.Clear();
            ExprTextBox.Focus();

            ClearButton.Visibility = Visibility.Collapsed;
            InitialText.Visibility = Visibility.Visible;

            curExpr = String.Empty;
        }

        #endregion

        private void HighlightLine(RichTextBox textBox, string line)
        {
            Run run = new Run(line);
            Paragraph paragraph = new Paragraph();
            paragraph.Inlines.Add(run);

            SyntaxHighlighting.Highlight(run);

            textBox.Document.Blocks.Add(paragraph);
        }

        #region textbox change

        private string GetExpr()
        {
            TextRange range = new TextRange(ExprTextBox.Document.ContentStart, ExprTextBox.Document.ContentEnd);

            return range.Text.TrimEnd('\r', '\n');
        }

        private void RestoreCaretPosition()
        {
            TextPointer caret = ExprTextBox.Document.ContentStart.GetInsertionPosition(LogicalDirection.Forward);
            while (caretPos-- != 0)
            {
                caret = caret.GetNextInsertionPosition(LogicalDirection.Forward);
            }
            ExprTextBox.CaretPosition = caret;
        }

        private void ChangeControlsVisibility(string expr)
        {
            if (String.IsNullOrEmpty(curExpr))
            {
                ClearButton.Visibility = Visibility.Visible;
                InitialText.Visibility = Visibility.Hidden;
            }
            else if (String.IsNullOrEmpty(expr))
            {
                ClearButton.Visibility = Visibility.Collapsed;
                InitialText.Visibility = Visibility.Visible;
            }
        }

        private void ExprTextBox_TextChanged(object sender, System.Windows.Controls.TextChangedEventArgs e)
        {
            if (ExprTextBox.Document.Blocks.Count == 0) return;

            string expr = GetExpr();

            if (expr == curExpr) return;

            ChangeControlsVisibility(expr);

            TextRange range = new TextRange(ExprTextBox.Document.ContentStart, ExprTextBox.CaretPosition);
            caretPos = range.Text.Length;

            ExprTextBox.Document.Blocks.Clear();

            curExpr = expr;

            HighlightLine(ExprTextBox, expr);

            RestoreCaretPosition();
        }

        #endregion

        #region interpreter commands

        private void InterpreterCommand_CanExecute(object sender, CanExecuteRoutedEventArgs e)
        {
            if (hsInt == null || hsInt.IsBusy || String.IsNullOrWhiteSpace(GetExpr()))
            {
                e.CanExecute = false;
            }
            else
            {
                e.CanExecute = true;
            }
        }

        private void NextStepCommand_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            ExprTextBox.Focus();

            if (lastExpr != curExpr)
            {
                OutputTextBox.Document.Blocks.Clear();
                lastExpr = curExpr;
            }

            new Task(() => hsInt.ShowNextStep(curExpr)).Start();
        }

        private void AllStepsCommand_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            ExprTextBox.Focus();

            if (lastExpr != curExpr)
            {
                OutputTextBox.Document.Blocks.Clear();
                lastExpr = curExpr;
            }

            new Task(() => hsInt.ShowAllSteps(curExpr)).Start();
        }

        #endregion

        private void HighlightResult(string result)
        {
            Dispatcher.BeginInvoke((Action)delegate
            {
                foreach (string line in result.Split(new string[] { Environment.NewLine }, StringSplitOptions.None))
                {
                    HighlightLine(OutputTextBox, line);
                }
            });
        }

        private void HsInt_OutputReceived(object sender, InterpreterEventArgs e)
        {
            HighlightResult(e.Result);
        }

        private void HsInt_ErrorReceived(object sender, InterpreterEventArgs e)
        {
            HighlightResult(e.Result);
        }

        #region menu items

        private void SamplesMenuItem_Click(object sender, RoutedEventArgs e)
        {
            SamplesWindow sw = new SamplesWindow();
            sw.Owner = this;
            sw.ShowDialog();
        }

        private void AboutMenuItem_Click(object sender, RoutedEventArgs e)
        {
            AboutWindow aw = new AboutWindow();
            aw.Owner = this;
            aw.ShowDialog();
        }

        #endregion
    }
}
