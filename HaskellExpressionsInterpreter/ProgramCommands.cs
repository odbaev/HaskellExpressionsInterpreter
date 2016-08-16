using System.Windows.Input;


namespace HaskellExpressionsInterpreter
{
    public static class ProgramCommands
    {
        private static RoutedUICommand exit;
        private static RoutedUICommand nextStep;
        private static RoutedUICommand allSteps;

        static ProgramCommands()
        {
            InputGestureCollection inputs;
            
            inputs = new InputGestureCollection();
            inputs.Add(new KeyGesture(Key.F4, ModifierKeys.Alt, "Alt+F4"));
            exit = new RoutedUICommand("Exit", "Exit", typeof(ProgramCommands), inputs);

            inputs = new InputGestureCollection();
            inputs.Add(new KeyGesture(Key.Enter, ModifierKeys.None, "Enter"));
            nextStep = new RoutedUICommand("NextStep", "NextStep", typeof(ProgramCommands), inputs);

            inputs = new InputGestureCollection();
            inputs.Add(new KeyGesture(Key.F5, ModifierKeys.None, "F5"));
            allSteps = new RoutedUICommand("AllSteps", "AllSteps", typeof(ProgramCommands), inputs);
        }
        
        public static RoutedUICommand Exit
        {
            get { return exit; }
        }

        public static RoutedUICommand NextStep
        {
            get { return nextStep; }
        }

        public static RoutedUICommand AllSteps
        {
            get { return allSteps; }
        }
    }
}
