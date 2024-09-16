// #define USE_CODE_GEN

using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Xml;

namespace FortranDriver
{

    static partial class Program
    {
        const string libraryName = "FortranDriverDLL";

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate void ActionRefInt([In] int progress, [In] int count);
        private static void StepUpdate(int i, int n)
            => Console.WriteLine($" Step: \t\t {i,2} of {n,2}");

        static void Main(string[] args)
        {
            Console.WriteLine("Calling Fortran from C#");
            Console.WriteLine();

            Console.WriteLine("Testing Native/Array Mathods");
            Console.WriteLine();
            TestNativeMethods();
            Console.Write("<paused>");
            Console.ReadLine();

            Console.WriteLine("Testing Fortran Vectors");
            Console.WriteLine();
            TestVectorMethods();
            Console.Write("<paused>");
            Console.ReadLine();

            Console.WriteLine("Testing Fortran Matrices");
            Console.WriteLine();
            TestMatrixMethods();
            Console.Write("<paused>");
            Console.ReadLine();

            Console.WriteLine("Testing Fortran Quaternion");
            Console.WriteLine();
            TestQuaternionMethods();
            Console.Write("<paused>");
            Console.ReadLine();
        }


        public static class FortranMethods
        {
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
        public static partial void DoWork(int n, int m, ref double A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);

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
        public static extern void DoWork(int rows, int columns, [In, Out] double[,] A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);

#endif

        #endregion
        }

        static void TestNativeMethods()
        {
            var callbackHandler = new ActionRefInt(StepUpdate);

            int n = 7, m = 3;
            double[,] A = HelperFunctions.BuildArray(n, m, (i, j) => m*(i-1.0) + j);
            Console.WriteLine();
            Console.WriteLine(" - Generate Matrix A in C#");
            A.ShowInConsole("A=", 6);

#if USE_CODE_GEN
            LibraryMethods.DoWork(n, m, ref A[0, 0], callbackHandler);
#else
            FortranMethods.DoWork(n, m, A, callbackHandler);
#endif

            Console.WriteLine(" - Manipulate Matrix A in Fortran");
            A.ShowInConsole("A=", 6);

        }

        static void TestVectorMethods()
        {
            const int s = 7;

            Console.WriteLine($"Vector Size ={s}");

            var e = NativeVector.Elemental(s, 1);

            Console.WriteLine("Elemental");
            Console.WriteLine($"e =\n{e}");

            var x = NativeVector.RandomMinMax(s, -1, 6);

            Console.WriteLine("Random");
            Console.WriteLine($"x =\n{x}");

            Console.WriteLine("Scaling/Negate");
            Console.WriteLine($"-x =\n{-x}");
            Console.WriteLine($"2x =\n{2*x}");
            Console.WriteLine($"x/2 =\n{x/2}");

            var y = e + x;
            var z = e - x;

            Console.WriteLine("Addition");
            Console.WriteLine($"e + x =\n{y}");
            Console.WriteLine("Subtraction");
            Console.WriteLine($"e - x =\n{z}");

            var d = NativeVector.Dot(y, z);
            var d_expect = 1 - x*x;

            Console.WriteLine("Dot Product");
            Console.WriteLine($"d = {d}");

            var M = NativeMatrix.Round( NativeMatrix.RandoMminMax(7, 7, -1.0, 6.0), 6);

            Console.WriteLine("Random Matrix");
            Console.WriteLine($"M =\n{M}");

            Console.WriteLine("Matrix/Vector Product");
            Console.WriteLine($"M*x =\n{M*x}");
            Console.WriteLine("Vector/Matrix Product");
            Console.WriteLine($"x*M =\n{x*M}");


            var A = NativeMatrix.FromRows(3, 3,
                5, -1, 0,
                1, 3, 44,
                -1, 0, 4);                

            Console.WriteLine("Matrix By Rows");
            Console.WriteLine($"A =\n{A}");

            var b = NativeVector.FromValues(3.0, 2.0, 1.0);
            Console.WriteLine("Vector From Values");
            Console.WriteLine($"b =\n{b}");

            var g = A.Solve(b);

            Console.WriteLine("Matrix/Vector Solve");
            Console.WriteLine($"g = b/A = \n{g}");            
            
            Console.WriteLine("Residual");
            Console.WriteLine($"b-A*g =\n{b - A*g}");            

        }
        static void TestMatrixMethods()
        {
            const int s = 7, t = 3;

            var r = NativeVector.Round( NativeVector.RandomMinMax(s, -1, 6), 6);

            var D = NativeMatrix.Diagonal(r.ToArray());
            var S = NativeMatrix.Scalar(s, s, 2.0);
            var R = NativeMatrix.RandoMminMax(s, s, -1, 6);
            Console.WriteLine("Scalar");
            Console.WriteLine($"S = \n{S}");
            Console.WriteLine("Diagonal");
            Console.WriteLine($"D = \n{D}");
            Console.WriteLine("Random");
            Console.WriteLine($"R = \n{R}");

            var A = R + D;
            var B = R - D;
            Console.WriteLine("Add");
            Console.WriteLine($"A = \n{A}");
            Console.WriteLine("Subtract");
            Console.WriteLine($"B = \n{B}");

            var N = -R;
            Console.WriteLine("Scale/Negate");
            Console.WriteLine($"N = \n{N}");

            var d = R.Determinant();
            Console.WriteLine("Determinant");
            Console.WriteLine($"d = \n{d}");

            var A_tr = NativeMatrix.Transpose(A);
            Console.WriteLine("Transpose");
            Console.WriteLine($"A_tr = \n{A_tr}");

            var H_1 = NativeMatrix.Round( NativeMatrix.RandoMminMax(42,1), 4);
            var H_2 = H_1.ReShape(21, 2);
            var H_3 = H_2.ReShape(7, 6);
            var H_4 = H_3.ReShape(3, 14);
            var H_5 = H_4.ReShape(1, 42);

            Console.WriteLine("Reshape");
            Console.WriteLine($"({H_1.Rows},{H_1.Columns}):");
            Console.WriteLine(H_1);
            Console.WriteLine($"({H_2.Rows},{H_2.Columns}):");
            Console.WriteLine(H_2);
            Console.WriteLine($"({H_3.Rows},{H_3.Columns}):");
            Console.WriteLine(H_3);
            Console.WriteLine($"({H_4.Rows},{H_4.Columns}):");
            Console.WriteLine(H_4);
            Console.WriteLine($"({H_5.Rows},{H_5.Columns}):");
            Console.WriteLine(H_5);

            Console.WriteLine("Slice");

            var S_1 = H_3.Slice(1, 4, 1, 3);
            var S_2 = H_3.Slice(1, 4, 4, 6);
            var S_3 = H_3.Slice(5, 7, 1, 3);
            var S_4 = H_3.Slice(5, 7, 4, 6);
            Console.WriteLine($"S_1 = ({S_1.Rows},{S_1.Columns}):");
            Console.WriteLine(S_1);
            Console.WriteLine($"S_2 = ({S_2.Rows},{S_2.Columns}):");
            Console.WriteLine(S_2);
            Console.WriteLine($"S_3 = ({S_3.Rows},{S_3.Columns}):");
            Console.WriteLine(S_3);
            Console.WriteLine($"S_4 = ({S_4.Rows},{S_4.Columns}):");
            Console.WriteLine(S_4);
            Console.WriteLine();

            Console.WriteLine("Index operator");

            Console.WriteLine($"S_4[1,1] = \n{S_4[1, 1]}\n");
            Console.WriteLine($"S_4[1..2, 2..3] = \n{S_4[1..2, 2..3]}");
            Console.WriteLine($"S_4[^1, 1] = \n{S_4[^1, 1]}\n");
            Console.WriteLine($"S_4[^2..^1, ^1] = \n{S_4[^2..^1, ^1]}");
            Console.WriteLine($"S_4[^1, ^3..^2] = \n{S_4[^1, ^3..^2]}");
            Console.WriteLine($"S_4[1..2, ^2..^1] = \n{S_4[1..2, ^2..^1]}");
            Console.WriteLine();

            {
                A = NativeMatrix.FromRows(3, 3,
                    5, -1, 0,
                    1, 3, 44,
                    -1, 0, 4);

                Console.WriteLine("Test Matrix");
                Console.WriteLine($"A = \n{A}");
            }

            var A_inv = A.Inverse();
            Console.WriteLine("Inverse");
            Console.WriteLine($"A_inv = \n{A_inv}");

            Console.WriteLine("Check Identity");
            Console.WriteLine($"A_inv*A=\n{A_inv * A}");

            var Y = NativeMatrix.Round( NativeMatrix.RandoMminMax(s, t, -1, 6), 6);
            Console.WriteLine("Random");
            Console.WriteLine($"Y = \n{Y}");

            var M = NativeMatrix.Round( NativeMatrix.RandoMminMax(7, 7, -1.0, 6.0), 6);

            var G = M*Y;
            Console.WriteLine("Product G=M*Y");
            Console.WriteLine($"G=\n{G}");


            var U = M.Solve(G);
            Console.WriteLine("Solve M*U=G");
            Console.WriteLine($"U=\n{U}");

            Console.WriteLine("Residual");
            Console.WriteLine($"U-Y=\n{NativeMatrix.Round(U-Y, 6)}");

            int seed = Environment.TickCount;

            var C = NativeMatrix.Round( 6*NativeMatrix.Identity(6) - NativeMatrix.RandomUniform(6, 6, ref seed), 6);
            var e = NativeVector.Round(NativeVector.RandomUniform(6), 6);
            var xs = C.Solve(e);
            var xb = C.BlockSolve(e);

            Console.WriteLine("Uniform Matrix.");
            Console.WriteLine($"C = \n{C}");
            Console.WriteLine("RHS Vector.");
            Console.WriteLine($"e = \n{e}");

            var es = e - C*xs;
            var eb = e - C*xb;

            Console.WriteLine("Serial LU decomposition.");
            Console.WriteLine($"xs = \n{xs}");
            Console.WriteLine("Serial Residual.");
            Console.WriteLine($"es = \n{ NativeVector.Round( es, 6)}");


            Console.WriteLine("Block LU decomposition.");
            Console.WriteLine($"xb = \n{xb}");
            Console.WriteLine("Block Residual.");
            Console.WriteLine($"eb = \n{ NativeVector.Round( eb, 6)}");

        }

        static void TestQuaternionMethods()
        {
            NativeQuaternion.TestNativeQuaternion();

            
        }


    }
}
