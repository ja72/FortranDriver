// #define USE_CODE_GEN

using System;
using System.Reflection;
using System.Runtime.InteropServices;

namespace FortranDriver
{

    static partial class Program
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

            TestNativeMethods();

            Console.WriteLine();
            Console.WriteLine("Testing Native/Fortran Vectors & Matrices");

            TestVectorMethods();
            TestMatrixMethods();
            TestQuaternionMethods();
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

            var n = -x;

            Console.WriteLine("Scaling/Negate");
            Console.WriteLine($"n =\n{n}");
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

            var A = NativeMatrix.Random(s, s, -1, 6);

            Console.WriteLine("Random");
            Console.WriteLine($"A =\n{A}");

            var g = A*x;
            var f = x*A;

            Console.WriteLine("Matrix/Vector Product");
            Console.WriteLine($"g =\n{g}");
            Console.WriteLine("Vector/Matrix Product");
            Console.WriteLine($"f =\n{f}");

            var u = g/A;

            Console.WriteLine("Matrix/Vector Solve");
            Console.WriteLine($"u =\n{u}");            
            
            Console.WriteLine("Residual");
            Console.WriteLine($"u-x =\n{u-x}");            
        }
        static void TestMatrixMethods()
        {
            const int s = 7, t = 3;

            var r = NativeVector.RandomMinMax(s, -1, 6);

            var D = NativeMatrix.Diagonal(r.ToArray());
            var S = NativeMatrix.Scalar(s, s, 2.0);
            var R = NativeMatrix.Random(s, s, -1, 6);
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

            var d = NativeMatrix.Determinant(R);
            Console.WriteLine("Determinant");
            Console.WriteLine($"d = \n{d}");

            var A_tr = NativeMatrix.Transpose(A);
            Console.WriteLine("Transpose");
            Console.WriteLine($"A_tr = \n{A_tr}");

            var H_1 = NativeMatrix.Random(42,1);
            var H_2 = NativeMatrix.Reshape(H_1, 21, 2);
            var H_3 = NativeMatrix.Reshape(H_2, 7, 6);
            var H_4 = NativeMatrix.Reshape(H_3, 3, 14);
            var H_5 = NativeMatrix.Reshape(H_4, 1, 42);

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

            var S_1 = NativeMatrix.Slice(H_3, 1, 4, 1, 3);
            var S_2 = NativeMatrix.Slice(H_3, 1, 4, 4, 6);
            var S_3 = NativeMatrix.Slice(H_3, 5, 7, 1, 3);
            var S_4 = NativeMatrix.Slice(H_3, 5, 7, 4, 6);
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

            var A_inv = NativeMatrix.Inverse(A);
            Console.WriteLine("Inverse");
            Console.WriteLine($"A_inv = \n{A_inv}");

            var I = A_inv * A;

            Console.WriteLine("Check Identity");
            Console.WriteLine($"A_inv*A=\n{I}");
            Console.WriteLine($"A_inv*A-1=\n{I-1}");

            var Y = NativeMatrix.Random(s, t, -1, 6);
            Console.WriteLine("Random");
            Console.WriteLine($"Y = \n{Y}");

            var G = A*Y;
            Console.WriteLine("Product G=A*Y");
            Console.WriteLine($"G=\n{G}");

            var U = G/A;
            Console.WriteLine("Solve A*U=G");
            Console.WriteLine($"U=\n{U}");

            Console.WriteLine("Residual");
            Console.WriteLine($"U-Y=\n{U-Y}");

        }

        static void TestQuaternionMethods()
        {
            NativeQuaternion.TestNativeQuaternion();

            
        }


    }
}
