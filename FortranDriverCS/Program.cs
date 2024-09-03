// #define USE_CODE_GEN

using System;
using System.Reflection;
using System.Runtime.InteropServices;

namespace FortranDriver
{

    partial class Program
    {
        const string libraryName = "FortranDriverDLL";

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate void ActionRefInt(int progress, int count);
        private static void StepUpdate(int i, int n)
        {
            Console.WriteLine($" Step: \t\t {i,2} of {n,2}");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("Calling Fortran from C#");
            Console.WriteLine();

            var callbackHandler = new ActionRefInt(StepUpdate);

            int n = 7, m = 3;
            double[,] A = HelperFunctions.BuildArray(n, m, (i, j) => m*(i-1.0) + j);
            Console.WriteLine();
            Console.WriteLine("1. Generate Matrix A in C#");
            Console.WriteLine("A=");
            A.ShowInConsole(6);

            double[] x = HelperFunctions.BuildArray(m, (i) => i/2.0);
            double[] b = new double[n], g = new double[n];


#if USE_CODE_GEN
            DoWork(n, m, ref A[0, 0], callbackHandler);
            array_product_mv(n, m, ref A[0, 0], ref x[0], ref b[0]);
            array_solve_mv(n, m, ref A[0, 0], ref b[0], ref g[0]);
#else
            DoWork(n, m, A, callbackHandler);
            array_product_mv(n, m, A, x, b);
            array_solve_mv(n, m, A, b, g);
#endif

            Console.WriteLine("2. Generate Vector x in C#");
            Console.WriteLine("x=");
            x.ShowInConsole(6);

            Console.WriteLine("3. Manipulate Matrix A in Fortran");
            Console.WriteLine("A=");
            A.ShowInConsole(6);

            Console.WriteLine("4. Calculate Vector b in Fortran");
            Console.WriteLine("b=");
            b.ShowInConsole(6);

            Console.WriteLine("5. Calculate Vector x in Fortran");
            Console.WriteLine("g=");
            g.ShowInConsole(6);

            Console.WriteLine();
            Console.WriteLine("Testing Native/Fortran Vectors & Matrices");

            NativeVector.TestMethods();
            NativeMatrix.TestMethods();
        }


        #region Native Methods

#if USE_CODE_GEN
        /// <summary>
        /// Fortran DLL call to manipulate matrix <paramref name="A"/>
        /// </summary>
        /// <param name="rows">The n.</param>
        /// <param name="columns">The n.</param>
        /// <param name="A">The matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="callBack">The call-back function to report on progress.</param>
        [LibraryImport(libraryName, EntryPoint = "DoWork", StringMarshalling = StringMarshalling.Utf8)]
        //[UnmanagedCallConv( CallConvs = new[] { typeof(CallConvCdecl) })]
        static partial void DoWork(int n, int m, ref double A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);

        /// <summary>
        /// Fortran DLL call to matrix multiply.
        /// </summary>
        /// <param name="A">The coefficient matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="x">The known vector</param>
        /// <param name="b">The unknown vector</param>
        [LibraryImport(libraryName, EntryPoint = "array_product_mv")]
        static partial void array_product_mv(int rows, int columns, ref double A, ref double x, ref double b);

        /// <summary>
        /// Fortran DLL call to solve a linear system of equations.
        /// </summary>
        /// <param name="A">The coefficient matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="b">The known vector</param>
        /// <param name="x">The unknown vector</param>
        [LibraryImport(libraryName, EntryPoint = "array_solve_mv")]
        static partial void array_solve_mv(int rows, int columns, ref double A, ref double b, ref double x);
#else
        /// <summary>
        /// Fortran DLL call to manipulate matrix <paramref name="A"/>
        /// </summary>
        /// <param name="rows">The n.</param>
        /// <param name="columns">The n.</param>
        /// <param name="A">The matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="callBack">The call-back function to report on progress.</param>
        [DllImport(libraryName, EntryPoint = "DoWork", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void DoWork(int rows, int columns, [In, Out] double[,] A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>b=A*x</code>.
        /// </summary>
        /// <param name="A">The coefficient matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="x">The known vector</param>
        /// <param name="b">The result vector</param>
        [DllImport(libraryName, EntryPoint = "array_product_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_product_mv(int rows, int columns, [In] double[,] A, [In] double[] x, [In, Out] double[] b);


        /// <summary>
        /// Fortran DLL call to solve the linear system of equations <code>A*x=b</code> for <paramref name="x"/>.
        /// </summary>
        /// <param name="A">The coefficient matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="b">The known vector</param>
        /// <param name="x">The unknown vector</param>
        [DllImport(libraryName, EntryPoint = "array_solve_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void array_solve_mv(int rows, int columns, [In] double[,] A, [In] double[] b, [In, Out] double[] x);
#endif

        #endregion

    }
}
