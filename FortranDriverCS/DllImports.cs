using System.Runtime.InteropServices;

namespace FortranDriver
{
    public static class DllImports
    {
        const string dllFile = "FortranDriverDLL.dll";

        // Delegate type.
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        // Important the int is passed by ref (else we could use built-in Action<T> instead of delegate).
        public delegate void ActionRefInt(int progress, int count);

        public static void TestFortran()
        {
            Console.WriteLine("Call Fortran code declared with DllImport()");
            ActionRefInt callbackHandler = new ActionRefInt(OnUpdateProgress);
            Console.WriteLine();
            int n = 7, m = 3;
            Console.WriteLine("1. Generate Matrix A in C#");
            double[,] A = HelperFunctions.BuildArray(n, m, (i, j) => m*(i-1.0) + j);
            Console.WriteLine("A=");
            A.ShowInConsole(6);
            
            Console.WriteLine("2. Manipulate Matrix A in Fortran");
            DoWork(n, m, A, callbackHandler);
            Console.WriteLine("A=");
            A.ShowInConsole(6);

            Console.WriteLine("3. Generate Vector x in C#");
            double[] x = HelperFunctions.BuildArray(m, (i) => i/2.0);
            double[] b = new double[n];

            Console.WriteLine("x=");
            x.ShowInConsole(6);

            Console.WriteLine("4. Calculate Vector b in Fortran");
            Product1(n, m, A, x, b);
            Console.WriteLine("b=");
            b.ShowInConsole(6);

            Console.WriteLine("5. Calculate Vector x in Fortran");
            Solve1(n, m, A, b, x);
            Console.WriteLine("x=");
            x.ShowInConsole(6);

        }
        public static void OnUpdateProgress(int progress, int count)
        {
            Console.WriteLine($" Step: \t\t {progress,2} of {count,2}");
        }
        #region Unmanaged Code        
        /// <summary>
        /// Fortran DLL call to manipulate matrix <paramref name="A"/>
        /// </summary>
        /// <param name="rows">The n.</param>
        /// <param name="columns">The n.</param>
        /// <param name="A">The matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="callBack">The call-back function to report on progress.</param>
        [DllImport(dllFile, EntryPoint = "DoWork", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        static extern void DoWork(int rows, int columns, [In, Out] double[,] A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);

        /// <summary>
        /// Fortran DLL call to matrix multiply.
        /// </summary>
        /// <param name="x">The first vector.</param>
        /// <param name="y">The second vector.</param>
        /// <param name="z">The dot product result.</param>
        [DllImport(dllFile, EntryPoint = "Dot1", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        static extern void Dot1(int size, [In] double[] x, [In] double[] y, [Out] out double z);

        /// <summary>
        /// Fortran DLL call to matrix multiply.
        /// </summary>
        /// <param name="A">The coefficient matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="x">The known vector</param>
        /// <param name="b">The unknown vector</param>
        [DllImport(dllFile, EntryPoint = "Product1", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        static extern void Product1(int rows, int columns, [In] double[,] A, [In] double[] x, [In, Out] double[] b);
        /// <summary>
        /// Fortran DLL call to matrix multiply.
        /// </summary>
        /// <param name="A">The coefficient matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="x">The known vector</param>
        /// <param name="b">The unknown vector</param>
        [DllImport(dllFile, EntryPoint = "Product2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        static extern void Product2(int rows, int columns, int pages, [In] double[,] A, [In] double[,] x, [In, Out] double[,] b);
        /// <summary>
        /// Fortran DLL call to solve a linear system of equations.
        /// </summary>
        /// <param name="A">The coefficient matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="b">The known vector</param>
        /// <param name="x">The unknown vector</param>
        [DllImport(dllFile, EntryPoint = "Solve1", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        static extern void Solve1(int rows, int columns, [In] double[,] A, [In] double[] b, [In, Out] double[] x);
        /// <summary>
        /// Fortran DLL call to solve a linear system of equations.
        /// </summary>
        /// <param name="A">The coefficient matrix. Fortran requires matrices to be column major 
        /// but C# supplies row major matrices by default. Care must be taken to define them transposed 
        /// in C# before sending them to Fortran.</param>
        /// <param name="b">The known matrix. Fortran requires matrices to be column major 
        /// but C# supplies row major matrices by default. Care must be taken to define them transposed 
        /// in C# before sending them to Fortran.</param>
        /// <param name="x">The unknown matrix. Fortran requires matrices to be column major 
        /// but C# supplies row major matrices by default. Care must be taken to define them transposed 
        /// in C# before sending them to Fortran.</param>
        [DllImport(dllFile, EntryPoint = "Solve2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        static extern void Solve2(int rows, int columns, int pages, [In] double[,] A, [In] double[,] b, [In, Out] double[,] x);
        #endregion
    }
}
