// #define USE_CODE_GEN

using System;
using System.Diagnostics;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Xml;

namespace FortranDriver
{
    using static NativeVector;
    using static NativeMatrix;

    static partial class Program
    {

#if DEBUG
        const string libraryName = "FortranDriverDLL_d";
#else
        const string libraryName = "FortranDriverDLL";
#endif

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
            //Console.Write("<paused>");
            //Console.ReadLine();

            Console.WriteLine("Testing Fortran Vectors");
            Console.WriteLine();
            TestVectorMethods();
            //Console.Write("<paused>");
            //Console.ReadLine();

            Console.WriteLine("Testing Fortran Matrices");
            Console.WriteLine();
            TestMatrixMethods();
            //Console.Write("<paused>");
            //Console.ReadLine();

            Console.WriteLine("Testing Fortran Quaternion");
            Console.WriteLine();
            TestQuaternionMethods();
            //Console.Write("<paused>");
            //Console.ReadLine();

            BenchMatrixSolve();
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

            var e = Elemental(s, 1);

            Console.WriteLine("Elemental");
            Console.WriteLine($"e =\n{e}");

            var u = NativeVector.RandomMinMax(s, -1, 6);

            Console.WriteLine("Random");
            Console.WriteLine($"u =\n{u}");

            Console.WriteLine("Scaling/Negate");
            Console.WriteLine($"-u =\n{-u}");
            Console.WriteLine($"2u =\n{2*u}");
            Console.WriteLine($"u/2 =\n{u/2}");

            var y = e + u;
            var z = e - u;

            Console.WriteLine("Addition");
            Console.WriteLine($"e+u =\n{y}");
            Console.WriteLine("Subtraction");
            Console.WriteLine($"e-u =\n{z}");

            var d = Dot(y, z);
            var d_expect = 1 - u*u;

            Console.WriteLine("Dot Product");
            Console.WriteLine($"d = {d} (actual), {d_expect} (expected)");

            var M = Round(RandomMinMax(7, 7, -1.0, 6.0), 6);

            Console.WriteLine("Random Matrix");
            Console.WriteLine($"M =\n{M}");

            Console.WriteLine("Matrix/Vector Product");
            Console.WriteLine($"M*u =\n{Round(M*u,4)}");
            Console.WriteLine("Vector/Matrix Product");
            Console.WriteLine($"u*M =\n{Round(u*M,4)}");


            for (int size = 2; size <= 6; size++)
            {

                var A = Round(RandomMinMax(size, size, -1.0, 6.0), 6);

                Console.WriteLine($"Test Matrix ({size},{size})");
                Console.WriteLine($"A = \n{A}");

                var A_inv = A.Inverse();
                Console.WriteLine("Inverse");
                Console.WriteLine($"A_inv = \n{Round(A_inv,6)}");

                Console.WriteLine("Check Identity");
                Console.WriteLine($"A_inv*A=\n{Round(A_inv*A,4)}");

                NativeVector b = Round(RandomMinMax(size, -1.0, 6.0), 6);
                Console.WriteLine("RHS");
                Console.WriteLine($"b = \n{b}");

                NativeVector x = A.Solve(b);
                Console.WriteLine("Solve A*x=b");
                Console.WriteLine($"x=\n{x}");
                Console.WriteLine("Product A*x");
                Console.WriteLine($"\n{A*x}");
                Console.WriteLine("Residual b-A*x");
                Console.WriteLine($"\n{b-A*x}");
            }

        }
        static void TestMatrixMethods()
        {

            double[] values = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0];

            Console.WriteLine($"data = [{string.Join(",", values)}]");

            Console.WriteLine($"FromRows(2,3,data) = ");
            Console.WriteLine(NativeMatrix.FromRows(2, 3, values   ));
            Console.WriteLine($"FromColumns(2,3,data) = ");
            Console.WriteLine(NativeMatrix.FromColumns(2, 3, values));
            Console.WriteLine($"FromRows(3,2,data) = ");
            Console.WriteLine(NativeMatrix.FromRows(3, 2, values   ));
            Console.WriteLine($"FromColumns(3,2,data) = ");
            Console.WriteLine(NativeMatrix.FromColumns(3, 2, values));

            const int s = 7, t = 3;

            var r = Round( NativeVector.RandomMinMax(s, -1.0, 6.0), 6);

            var D = Diagonal(r.ToArray());
            var S = Scalar(s, s, 4.0/Math.PI);
            var R = Round(RandomMinMax(s, s, -1.0, 6.0), 4 );
            Console.WriteLine("Scalar");
            Console.WriteLine($"S = \n{Round(S, 6)}");
            Console.WriteLine("Diagonal");
            Console.WriteLine($"D = \n{Round(D, 6)}");
            Console.WriteLine("Random");
            Console.WriteLine($"R = \n{Round(R, 6)}");

            var A = Round( R + D, 6);
            var B = Round( R - D, 6);
            Console.WriteLine("Add");
            Console.WriteLine($"R + D = \n{Round(A, 6)}");
            Console.WriteLine("Subtract");
            Console.WriteLine($"R - D = \n{Round(B, 6)}");

            Console.WriteLine("Scale/Negate");
            Console.WriteLine($"-R = \n{-R}");
            Console.WriteLine($"2R = \n{2*R}");
            Console.WriteLine($"R/π = \n{Round(R/Math.PI, 6)}");

            var d = R.Determinant();
            Console.WriteLine("Determinant");
            Console.WriteLine($"d = \n{d}");

            var A_tr = Transpose(A);
            Console.WriteLine("Transpose");
            Console.WriteLine($"A_tr = \n{A_tr}");

            var H_1 = Round(RandomMinMax(42,1), 4);
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


            for (int size = 2; size <= 6; size++)
            {

                A = Round(RandomMinMax(size, size, -1.0, 6.0), 4);

                Console.WriteLine($"Test Matrix ({size},{size})");
                Console.WriteLine($"A = \n{A}");

                NativeMatrix A_inv = A.Inverse();
                Console.WriteLine("Inverse");
                Console.WriteLine($"A_inv = \n{Round(A_inv, 6)}");

                Console.WriteLine("Check Identity");
                Console.WriteLine($"A_inv*A=\n{Round(A_inv * A, 6)}");

                NativeMatrix b = Round(RandomMinMax(size, t, -1.0, 6.0), 6);
                Console.WriteLine("RHS");
                Console.WriteLine($"b = \n{b}");

                NativeMatrix x = A.Solve(b);
                Console.WriteLine("Solve A*x=b");
                Console.WriteLine($"x=\n{x}");
                Console.WriteLine("Product A*x");
                Console.WriteLine($"\n{Round(A*x,6)}");
                Console.WriteLine("Residual b-A*x");
                Console.WriteLine($"\n{Round(b-A*x,6)}");
            }

            int seed = Environment.TickCount;

            var C = Round( 6.0*Identity(6) - RandomUniform(6, 6, ref seed), 6);
            var e = Round(RandomUniform(6), 6);
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
            Console.WriteLine($"es = \n{Round( es, 6)}");


            Console.WriteLine("Block LU decomposition.");
            Console.WriteLine($"xb = \n{xb}");
            Console.WriteLine("Block Residual.");
            Console.WriteLine($"eb = \n{Round( eb, 6)}");

        }

        static void TestQuaternionMethods()
        {
            NativeQuaternion.TestNativeQuaternion();            
        }

        static void BenchMatrixSolve()
        {
#if DEBUG
            Console.WriteLine("Benchmark DEBUG Matrix Solver.");
#else
            Console.WriteLine("Benchmark RELEASE Matrix Solver.");
#endif
            Console.WriteLine();
            Console.WriteLine($"CPU#:{Environment.ProcessorCount}, USER:{Environment.UserName}, WIN:{Environment.OSVersion.VersionString}, {(Environment.Is64BitProcess ? "x64" : "x86")}");


            Console.WriteLine();
            Stopwatch sw = new Stopwatch();
            double t1_μs, t1_s, t2_μs, t2_s;

            // Burn In
            {
                NativeMatrix A = NativeMatrix.RandomMinMax(4, 4, -1.0, 6.0);
                NativeVector b = NativeVector.RandomMinMax(4, -3.0, 9.0);
                double sum = 0;
                for (int k = 0; k < 20000; k++)
                {
                    NativeVector g = A.Solve((k+1)*b);
                    sum += g[1];
                }
            }


            Console.WriteLine($"|=========== SERIAL ============== |=========== BLOCK =============== |");
            Console.Write($"| {"Size", 5} {"Time", 16} {"Rate",9} " );
            Console.WriteLine($"| {"Size", 5} {"Time", 16} {"Rate",9} |");
            Console.WriteLine($"|--------------------------------- |--------------------------------- |");
            Console.Write($"| {"[#]", 5} {"[s]", 16} {"[n^2/μs]",9} ");
            Console.WriteLine($"| {"[#]", 5} {"[s]", 16} {"[n^2/μs]",9} |");
            for (int i = -5; i < 14; i++)
            {
                int size = i>=0 ? (int)Math.Pow(5, (double)(i+1)/4+1) : i+7;
                NativeMatrix A = RandomMinMax(size, size, -1.0, 6.0);
                NativeVector b = RandomMinMax(size, -3.0, 9.0);

                int repeat = Math.Max(1, 1250000 / (size*size) );

                double sum = 0.0;
                NativeVector x = new NativeVector(size);
                NativeVector y = new NativeVector(size);
                sw.Reset();
                sw.Start();
                for (int iter = 0; iter < repeat; iter++)
                {
                    x = A.Solve(b);
                    sum += x[1];
                }
                
                sw.Stop();
                t1_μs = sw.Elapsed.TotalMicroseconds/repeat;
                t1_s = sw.Elapsed.TotalSeconds/repeat;
                sw.Reset();
                sw.Start();
                for (int iter = 0; iter < repeat; iter++)
                {
                    y = A.BlockSolve(b);
                    sum += x[1];
                }
                sw.Stop();
                t2_μs = sw.Elapsed.TotalMicroseconds/repeat;
                t2_s = sw.Elapsed.TotalSeconds/repeat;
                Console.Write($"| {size, 5} {t1_s, 16:g9} {(size*size)/t1_μs, 9:g4} ");
                Console.WriteLine($"| {size, 5} {t2_s, 16:g9} {(size*size)/t2_μs, 9:g4} |");
            }
        }


    }
}
