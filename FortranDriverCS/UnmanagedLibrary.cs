using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace FortranDriver
{
    public static partial class UnmanagedLibrary
    {
        [LibraryImport("FortranDriverDLL.dll", EntryPoint = "DoWork", StringMarshalling = StringMarshalling.Utf8)]
        [UnmanagedCallConv( CallConvs = new[] { typeof(CallConvCdecl) })]
        static partial void DoWork(ref int n, ref double A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);

        // Delegate type.
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        // Important the int is passed by ref (else we could use built-in Action<T> instead of delegate).
        public delegate void ActionRefInt(ref int progress, ref int count);

        public static void TestFortran()
        {
            Console.WriteLine("Call Fortran code declared with LibraryImport()");
            ActionRefInt callbackHandler = new ActionRefInt(OnUpdateProgress);
            Console.WriteLine();
            int n = 10;
            double[,] A = HelperFunctions.BuildArray(n, n, (i, j) => i==j ? i+1 : 0);
            A.ShowArray(5);
            
            DoWork(ref n, ref A[0,0], callbackHandler);

            A.ShowArray(5);
        }
        public static void OnUpdateProgress(ref int progress, ref int count)
        {
            Console.WriteLine($" Step: \t\t {progress,2} of {count,2}");
        }
    }
}
