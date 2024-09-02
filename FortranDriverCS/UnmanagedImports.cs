using System.Runtime.InteropServices;

namespace FortranDriver
{
    public static class UnmanagedImports
    {
        // Fortran DLL interface.
        [DllImport("FortranDriverDLL.dll", EntryPoint = "DoWork", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        public static extern void DoWork(ref int n, [In, Out] double[,] A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);

        // Delegate type.
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        // Important the int is passed by ref (else we could use built-in Action<T> instead of delegate).
        public delegate void ActionRefInt(ref int progress, ref int count);

        public static void TestFortran()
        {
            Console.WriteLine("Call Fortran code declared with DllImport()");
            ActionRefInt callbackHandler = new ActionRefInt(OnUpdateProgress);
            Console.WriteLine();
            int n = 10;
            double[,] A = HelperFunctions.BuildArray(n, n, (i, j) => i==j ? i+1 : 0);
            A.ShowArray(5);
            
            DoWork(ref n, A, callbackHandler);

            A.ShowArray(5);
        }
        public static void OnUpdateProgress(ref int progress, ref int count)
        {
            Console.WriteLine($" Step: \t\t {progress,2} of {count,2}");
        }
    }
}
