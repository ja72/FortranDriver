﻿// #define USE_CODE_GEN
//#define CHECK_EXPORTS
//#define CHECK_IMPORTS
#define TEST_OUTPUTS
#define RUN_BENCHMARK
//#define TEST_FORMS

using System;
using System.Diagnostics;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Xml;

using JA.Fortran;
using JA.Fortran.Arrays;
using JA.Fortran.Physics;

namespace JA
{

    static partial class Program
    {
        #region Fortran API
        const string libraryName = FortranMethods.libraryName;

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate void ActionRefInt([In] int progress, [In] int count);

        public readonly struct FVal
        {
            readonly double _value;

            public FVal(double x)
            {
                this._value=x;
            }
            public double Value => this._value;

            #region Formatting
            public override string ToString() 
                => ToString(HelperFunctions.DefaultFormatting);
            public string ToString(string formatting) 
                => ToString(formatting, null);
            public string ToString(string formatting, IFormatProvider formatProvider) 
                => _value.ToString(formatting, formatProvider);
            #endregion
        }

        [DllImport(libraryName, EntryPoint = "call_test_obj_set", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_test_obj_set(in FVal x);
        [DllImport(libraryName, EntryPoint = "call_test_obj_get", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_test_obj_get([Out] out FVal x);

        /// <summary>
        /// Fortran DLL call to manipulate matrix <paramref name="A"/>
        /// </summary>
        /// <param name="rows">The n.</param>
        /// <param name="columns">The n.</param>
        /// <param name="A">The matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="callBack">The call-back function to report on progress.</param>
        [DllImport(libraryName, EntryPoint = "call_test_dowork", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_test_dowork(int rows, int columns, [In, Out] double[,] A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);

        [DllImport(libraryName, EntryPoint = "call_quat_test_all", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_quat_test_all();

        #endregion

        public static readonly Random rng = new Random();

        [STAThread()]
        static void Main(string[] args)
        {

#if CHECK_IMPORTS


            IEnumerable<Type> types = [
                typeof(Program),
                typeof(FortranMethods),
                typeof(FVector)       ,
                typeof(FMatrix)       ,
                typeof(FSpline)       ,
                typeof(FVector2)      ,
                typeof(FVector3)      ,
                typeof(FQuat4)        ,
                typeof(FMatrix2)      ,
                typeof(FMatrix3)      ,
                typeof(RigidBody)     ,
                typeof(FVector6)      ,
                typeof(FMatrix6)      ,
                ];

            var methods = types.SelectMany( 
                (t)=> t.GetMethods(BindingFlags.NonPublic|BindingFlags.Static|BindingFlags.DeclaredOnly)
                .Select(m => m.Name)).Where( s=>!s.StartsWith('<'));

            string text = string.Join(Environment.NewLine, methods.OrderBy((v) => v));
            File.WriteAllText("imports.txt", text);
#endif
#if CHECK_EXPORTS
            var all = File.ReadLines("exports_dump.txt");
            var vars = all.Where((line) => line.Contains('='));
            text=string.Join(Environment.NewLine, vars.Select(item =>
            {
                int index = item.IndexOf('=');
                return item.Substring(index+1).Trim();
            }));
            File.WriteAllText("exports.txt", text);
#endif

#if TEST_OUTPUTS
            Console.WriteLine("Calling Fortran from C#");
            Console.WriteLine();

            Console.WriteLine("Testing Native/Array Mathods");
            Console.WriteLine();
            TestNativeMethods();

            Console.WriteLine("Testing Fortran Vectors");
            Console.WriteLine();
            TestVectorMethods();

            Console.WriteLine("Testing Fortran Matrices");
            Console.WriteLine();
            TestMatrixMethods();

            //Console.WriteLine("Testing Fortran Quaternion");
            //Console.WriteLine();
            //TestQuaternionMethods();

            Console.WriteLine("Testing Fixed Vectors");
            TestFixedVectorMethods();

            Console.WriteLine("Testing Fixed Quaternion");
            Console.WriteLine();
            TestFixedQuatMethods();

            Console.WriteLine("Testing Layout Matrix");
            Console.WriteLine();
            TestLayoutMatrix();

            //Console.WriteLine("Testing Rigid Body Mechanics");
            //TestRigidBodyMethods();

#endif

#if RUN_BENCHMARK
            // BenchMatrixMultiply();
            Console.WriteLine("System Solution Bench");
            BenchMatrixSolve();
#endif

#if TEST_FORMS
            Application.Run(new RunningForm1());
#endif

        }


        #region TESTING METHODS


        static void TestNativeMethods()
        {
            FVal x= new FVal(double.Pi/4);

            Program.call_test_obj_set(x);
            Program.call_test_obj_get(out var x_get);

            Console.WriteLine($"Set internal value to {x}");
            Console.WriteLine($"Get internal value as {x_get}");

            static void StepUpdate(int i, int n)
            => Console.WriteLine($" Step: \t\t {i,2} of {n,2}");

            var callbackHandler = new ActionRefInt(StepUpdate);

            int n = 7, m = 3;
            double[,] A = HelperFunctions.BuildArray(n, m, (i, j) => m*(i-1.0) + j);
            Console.WriteLine();
            Console.WriteLine(" - Generate Matrix A in C#");
            const int Width = 12;
            A.ShowInConsole("A=", Width);

#if USE_CODE_GEN
            LibraryMethods.call_test_dowork(n, m, ref A[0, 0], callbackHandler);
#else
            Program.call_test_dowork(n, m, A, callbackHandler);
#endif

            Console.WriteLine(" - Manipulate Matrix A in Fortran");
            A.ShowInConsole("A=", Width);

        }
        static void TestVectorMethods()
        {
            const int s = 7;

            Console.WriteLine($"Vector Size ={s}");

            var e = FVector.Elemental(s, 1);

            Console.WriteLine("Elemental");
            Console.WriteLine($"e =\n{e}");

            var u = FVector.RandomMinMax(s, -1, 6);

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

            var d = FVector.Dot(y, z);
            var d_expect = 1 - u*u;

            Console.WriteLine("Dot Product");
            Console.WriteLine($"d = {d} (actual), {d_expect} (expected)");

            var M = FMatrix.Round(FMatrix.RandomMinMax(7, 7, -1.0, 6.0), 6);

            Console.WriteLine("Random Matrix");
            Console.WriteLine($"M =\n{M}");

            Console.WriteLine("Matrix/Vector Product");
            Console.WriteLine($"M*u =\n{FVector.Round(M*u, 4)}");
            Console.WriteLine("Vector/Matrix Product");
            Console.WriteLine($"u*M =\n{FVector.Round(u*M, 4)}");
            int[] sizes = [2, 3, 4, 5, 6, 12, 24, 72, 144, 512];
            const int sz_limit = 16;
            foreach (var size in sizes)
            {
                var A = FMatrix.RandomMinMax(size, size, -1.0, 6.0);

                Console.WriteLine($"Test Matrix ({size},{size})");
                if (size<=sz_limit)
                {
                    Console.WriteLine($"A = \n{FMatrix.Round(A, 6)}");
                }

                var A_inv = A.Inverse();
                Console.WriteLine("Inverse");
                if (size<=sz_limit)
                {
                    Console.WriteLine($"A_inv = \n{FMatrix.Round(A_inv, 6)}");
                }

                Console.WriteLine("Check Identity");
                if (size<=sz_limit)
                {
                    Console.WriteLine($"A_inv*A=\n{FMatrix.Round(A_inv*A, 4)}");
                }

                FVector b = FVector.RandomMinMax(size, -1.0, 6.0);
                Console.WriteLine("RHS");
                Console.WriteLine($"b = \n{b}");

                FVector x = A.Solve(b);
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
            Console.WriteLine(FMatrix.FromRows(2, 3, values));
            Console.WriteLine($"FromColumns(2,3,data) = ");
            Console.WriteLine(FMatrix.FromColumns(2, 3, values));
            Console.WriteLine($"FromRows(3,2,data) = ");
            Console.WriteLine(FMatrix.FromRows(3, 2, values));
            Console.WriteLine($"FromColumns(3,2,data) = ");
            Console.WriteLine(FMatrix.FromColumns(3, 2, values));

            const int s = 7, t = 3;

            var r = FVector.Round(FVector.RandomMinMax(s, -1.0, 6.0), 6);

#pragma warning disable IDE0305 // Simplify collection initialization
            var D = FMatrix.Diagonal(r.ToArray());
#pragma warning restore IDE0305 // Simplify collection initialization
            var S = FMatrix.Scalar(s, s, 4.0/Math.PI);
            var R = FMatrix.Round(FMatrix.RandomMinMax(s, s, -1.0, 6.0), 4);
            Console.WriteLine("Scalar");
            Console.WriteLine($"S = \n{FMatrix.Round(S, 6)}");
            Console.WriteLine("Diagonal");
            Console.WriteLine($"D = \n{FMatrix.Round(D, 6)}");
            Console.WriteLine("Random");
            Console.WriteLine($"R = \n{FMatrix.Round(R, 6)}");

            var A = FMatrix.Round(R + D, 6);
            var B = FMatrix.Round(R - D, 6);
            Console.WriteLine("Add");
            Console.WriteLine($"R + D = \n{FMatrix.Round(A, 6)}");
            Console.WriteLine("Subtract");
            Console.WriteLine($"R - D = \n{FMatrix.Round(B, 6)}");

            Console.WriteLine("Scale/Negate");
            Console.WriteLine($"-R = \n{-R}");
            Console.WriteLine($"2R = \n{2*R}");
            Console.WriteLine($"R/π = \n{FMatrix.Round(R/Math.PI, 6)}");

            var d = R.Determinant();
            Console.WriteLine("Determinant");
            Console.WriteLine($"d = \n{d}");

            var A_tr = FMatrix.Transpose(A);
            Console.WriteLine("Transpose");
            Console.WriteLine($"A_tr = \n{A_tr}");

            var H_1 = FMatrix.Round(FMatrix.RandomMinMax(42, 1), 4);
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

            var e_1 = FVector.Elemental(6,1);
            var e_2 = FVector.Elemental(6,2);
            var e_3 = FVector.Elemental(6,6);
            Console.WriteLine($"e_1 = \n{e_1}");
            Console.WriteLine($"e_2 = \n{e_2}");
            Console.WriteLine($"e_3 = \n{e_3}");

            var E123 = FMatrix.FromColumns(e_1, e_2, e_3);
            Console.WriteLine($"{{e_1|e_2|e_3}} = \n{E123}");

            var G123 = FMatrix.FromRows(e_1, e_2, e_3);
            Console.WriteLine($"{{e_1&e_2&e_3}} = \n{G123}");

            for (int size = 2; size<=6; size++)
            {

                A=FMatrix.Round(FMatrix.RandomMinMax(size, size, -1.0, 6.0), 4);

                Console.WriteLine($"Test Matrix ({size},{size})");
                Console.WriteLine($"A = \n{A}");

                FMatrix A_inv = A.Inverse();
                Console.WriteLine("Inverse");
                Console.WriteLine($"A_inv = \n{FMatrix.Round(A_inv, 6)}");

                Console.WriteLine("Check Identity");
                Console.WriteLine($"A_inv*A=\n{FMatrix.Round(A_inv*A, 6)}");

                FMatrix b = FMatrix.Round(FMatrix.RandomMinMax(size, t, -1.0, 6.0), 6);
                Console.WriteLine("RHS");
                Console.WriteLine($"b = \n{b}");

                FMatrix x = A.Solve(b);
                Console.WriteLine("Solve A*x=b");
                Console.WriteLine($"x=\n{x}");
                Console.WriteLine("Product A*x");
                Console.WriteLine($"\n{FMatrix.Round(A*x, 6)}");
                Console.WriteLine("Residual b-A*x");
                Console.WriteLine($"\n{FMatrix.Round(b-A*x, 6)}");
            }

            var C = FMatrix.Round(6.0*FMatrix.Identity(6) - FMatrix.RandomUniform(6, 6, ref FortranMethods.seed), 6);
            var e = FVector.Round(FVector.RandomUniform(6), 6);
            var xs = C.Solve(e);

            Console.WriteLine("Uniform Matrix.");
            Console.WriteLine($"C = \n{C}");
            Console.WriteLine("RHS Vector.");
            Console.WriteLine($"e = \n{e}");

            var es = e - C*xs;

            Console.WriteLine("Serial LU decomposition.");
            Console.WriteLine($"xs = \n{xs}");
            Console.WriteLine("Serial Residual.");
            Console.WriteLine($"es = \n{FVector.Round(es, 6)}");

        }
        static void TestFixedVectorMethods()
        {
            {
                Console.WriteLine("### Initialize some FVector2 variables");
                FVector2 v = new FVector2(1,2);
                FVector2 u = new FVector2(3,5);
                Console.WriteLine($"v= \n{v}");
                Console.WriteLine($"u= \n{u}");

                Console.WriteLine($"u.AsSpan().ToArray()={{{string.Join(',', u.AsSpan().ToArray())}}}");

                Console.WriteLine($"-v= \n{-v}");
                Console.WriteLine($"2u= \n{2*u}");
                Console.WriteLine($"u+v=\n{u+v}");
                Console.WriteLine($"u-v=\n{u-v}");
                Console.WriteLine($"u|v=\n{u|v}");
                var r = FVector2.Uniform(ref FortranMethods.seed);
                Console.WriteLine($"Uniform = \n{r}");
                Console.WriteLine($"Norm = {r.Norm()}");
                Console.WriteLine();
            }
            {
                Console.WriteLine("### Initialize some FVector3 variables");
                FVector3 v = new FVector3(1,2,-1);
                FVector3 u = new FVector3(3,5,2);
                Console.WriteLine($"v= \n{v}");
                Console.WriteLine($"u= \n{u}");

                Console.WriteLine($"u.AsSpan().ToArray()={{{string.Join(',', u.AsSpan().ToArray())}}}");

                Console.WriteLine($"-v= \n{-v}");
                Console.WriteLine($"2u= \n{2*u}");
                Console.WriteLine($"u+v=\n{u+v}");
                Console.WriteLine($"u-v=\n{u-v}");
                Console.WriteLine($"u|v=\n{u|v}");
                var r = FVector3.Uniform(ref FortranMethods.seed);
                Console.WriteLine($"Uniform = \n{r}");
                Console.WriteLine($"Norm = {r.Norm()}");
                Console.WriteLine();
            }
            {
                Console.WriteLine("### Initialize some FMatrix2 variables");
                FMatrix2 a = new FMatrix2(1,2,-1,0.25);
                FMatrix2 b = new FMatrix2(3,5,2,-0.75);
                Console.WriteLine($"a= \n{a}");
                Console.WriteLine($"b= \n{b}");

                Console.WriteLine($"u.AsSpan().ToArray()={{{string.Join(',', b.AsSpan().ToArray())}}}");

                Console.WriteLine($"-a= \n{-a}");
                Console.WriteLine($"2b= \n{2*b}");
                Console.WriteLine($"b+a=\n{b+a}");
                Console.WriteLine($"b-a=\n{b-a}");
                Console.WriteLine($"b*a=\n{b*a}");
                Console.WriteLine($"b|a=\n{b|a}");
                Console.WriteLine($"trace(b)=\n{b.Trace()}");
                Console.WriteLine($"det(b)=\n{b.Determinant()}");
                Console.WriteLine($"inv(b)=\n{b.Inverse()}");
                Console.WriteLine($"det(b)*inv(b)=\n{b.Determinant()*b.Inverse()}");
                Console.WriteLine($"inv(b)*b=\n{b.Inverse()*b}");
                FVector2 u = new FVector2(3,5);
                Console.WriteLine($"u= \n{u}");
                FVector2 v = b.Solve(u);
                Console.WriteLine($"v=solve(b,u)=\n{v}"); // u = b*v
                Console.WriteLine($"residual=u-b*v=\n{u-b*v}");
                var r = FMatrix2.Uniform(ref FortranMethods.seed);
                Console.WriteLine($"Uniform = \n{r}");
                Console.WriteLine();
            }
            {
                Console.WriteLine("### Initialize some FMatrix3 variables");
                FMatrix3 a = new FMatrix3(1,2,-1,0.25,-0.5,6,2.7,-1.4,3);
                FMatrix3 b = new FMatrix3(3,5,2,-0.75,0.4,2.15,5.5,0,1);
                Console.WriteLine($"a= \n{a}");
                Console.WriteLine($"b= \n{b}");

                Console.WriteLine($"u.AsSpan().ToArray()={{{string.Join(',', b.AsSpan().ToArray())}}}");

                Console.WriteLine($"-a= \n{-a}");
                Console.WriteLine($"2b= \n{2*b}");
                Console.WriteLine($"b+a=\n{b+a}");
                Console.WriteLine($"b-a=\n{b-a}");
                Console.WriteLine($"b*a=\n{b*a}");
                Console.WriteLine($"b|a=\n{b|a}");
                Console.WriteLine($"trace(b)=\n{b.Trace()}");
                Console.WriteLine($"det(b)=\n{b.Determinant()}");
                Console.WriteLine($"inv(b)=\n{b.Inverse()}");
                Console.WriteLine($"det(b)*inv(b)=\n{b.Determinant()*b.Inverse()}");
                Console.WriteLine($"inv(b)*b=\n{b.Inverse()*b}");
                FVector3 u = new FVector3(3,5,0.25);
                Console.WriteLine($"u={u}");
                FVector3 v = b.Solve(u);
                Console.WriteLine($"v=solve(b,u)=\n{v}"); // u = b*v
                Console.WriteLine($"residual=u-b*v=\n{u-b*v}");
                var r = FMatrix3.Uniform(ref FortranMethods.seed);
                Console.WriteLine($"Uniform = \n{r}");
                Console.WriteLine();
            }
        }
        static void TestFortranQuaternion()
        {
            Program.call_quat_test_all();
        }
        static void TestFixedQuatMethods()
        {
            {
                Console.WriteLine("### Initialize some FQuat4 variables");
                FQuat4 v = new FQuat4(1,2,-1,4);
                FQuat4 u = new FQuat4(3,5,2,0);
                Console.WriteLine($"v= \n{v}");
                Console.WriteLine($"u= \n{u}");

                Console.WriteLine($"u.AsSpan().ToArray()={{{string.Join(',', u.AsSpan().ToArray())}}}");

                Console.WriteLine($"-v= \n{-v}");
                Console.WriteLine($"2u= \n{2*u}");
                Console.WriteLine($"u+v=\n{u+v}");
                Console.WriteLine($"u-v=\n{u-v}");
                Console.WriteLine($"u|v=\n{u|v}");
                Console.WriteLine($"u^v=\n{u^v}");

                var r = FQuat4.Uniform(ref FortranMethods.seed);
                Console.WriteLine($"Uniform = \n{r}");
                Console.WriteLine($"Norm = {r.Norm()}");
                Console.WriteLine();
            }
            {
                Console.WriteLine("### Test some FQuat4 rotations");
                var axis = FVector3.Uniform(ref FortranMethods.seed);
                var angle = double.Pi * rng.NextDouble();

                FQuat4 q1 = FQuat4.FromRotation(axis, angle);
                Console.WriteLine($"Axis = \n{axis}, Angle = {180*angle/double.Pi} deg");
                Console.WriteLine($"q1 = \n{q1}, mag = {q1.Norm()}");
                Console.WriteLine();

                Console.WriteLine("Rotation Matrix");
                FMatrix3 R = q1.ToRotation();
                Console.WriteLine(R);

                Console.WriteLine("Quaternion From Rotation");
                FQuat4 q2 = FQuat4.FromRotation(R);
                Console.WriteLine($"q2 = \n{q2}, mag = {q2.Norm()}");
                Console.WriteLine();
                Console.WriteLine("Quaternion to Axis/Angle");
                q2.ToAxisAngle(out axis, out angle);
                Console.WriteLine($"Axis = \n{axis}, Angle = {180*angle/double.Pi} deg");
            }
            Console.WriteLine();
        }
        static void TestRigidBodyMethods()
        {

            RigidBody rb = new RigidBody(1.0,
                0.0644, 0.0644, 0.0322);

            double t = 0.0;

            FVector3 pos = FVector3.Zero;
            FQuat4 ori = FQuat4.Identity;
            FVector3 vee = 5*FVector3.EX;
            FVector3 omg = FVector3.EZ;

            Console.WriteLine("Position = ");
            Console.WriteLine(pos.ToString("g5"));
            Console.WriteLine("Orienation = ");
            Console.WriteLine(ori.ToString("g5"));
            Console.WriteLine("Velocity = ");
            Console.WriteLine(vee.ToString("g5"));
            Console.WriteLine("Rot. Velocity = ");
            Console.WriteLine(omg.ToString("g5"));
            var y = new FVector(13);
            rb.GetState(pos, ori, vee, omg, ref y);

            Console.WriteLine($"State Vector =\n{y}");
            var yp = new FVector(13);
            rb.GetStateDerivative(t, y, ref yp);

            Console.WriteLine($"State Vector Derivative =\n{yp}");

            double h = 0.05;
            var y_next = y + h * yp;

            Console.WriteLine($"Next Vector =\n{y_next}");

            rb.SetState(y_next, out pos, out ori, out vee, out omg);

            Console.WriteLine("Position = ");
            Console.WriteLine(pos.ToString("g5"));
            Console.WriteLine("Orienation = ");
            Console.WriteLine(ori.ToString("g6"));
            Console.WriteLine("Velocity = ");
            Console.WriteLine(vee.ToString("g5"));
            Console.WriteLine("Rot. Velocity = ");
            Console.WriteLine(omg.ToString("g5"));
        }
        static void TestLayoutMatrix()
        {

            var vec = new FPartitionedVector(1, 3, 2);

            var a = vec[1] = FVector.FromValues(HelperFunctions.RNG.NextDouble());
            var b = vec[2] = FVector3.Uniform(ref FortranMethods.seed);
            var c = vec[3] = FVector2.Uniform(ref FortranMethods.seed);

            Console.WriteLine($"vec =\n{vec}");

            var vec2 = new FPartitionedVector(a, b, c);

            Console.WriteLine($"vec2 =\n{vec2}");

        }
        static void BenchMatrixMultiply()
        {
            const int size = 2000; // Adjust this size to change runtime

            Console.WriteLine($"Matrix Multiplication Bench (size={size})");
            Console.WriteLine();
            var sw = Stopwatch.StartNew();
            double[,] matrixA = GenerateMatrix(size);
            var t1 = sw.Elapsed.TotalSeconds;
            Console.WriteLine($"CSharp Time to 1st generate matrix {t1:g6} sec");
            sw.Restart();
            double[,] matrixB = GenerateMatrix(size);
            var t2 = sw.Elapsed.TotalSeconds;
            Console.WriteLine($"CSharp Time to 2nd generate matrix {t2:g6} sec");
            sw.Restart();
            double[,] result = MultiplyMatrices(matrixA, matrixB, size);
            var t3 = sw.Elapsed.TotalSeconds;
            Console.WriteLine($"CSharp Time to do matrix multiply  {t3:g6} sec");
            sw.Stop();
            var nativeResult = new FMatrix(result, ElementOrder.ByRow);

            // Optional: Output a single value to prevent optimizations
            Console.WriteLine($"CSharp Result[0,0]: {result[0, 0]}");
            Console.WriteLine();
            sw.Restart();
            var nativeA = new FMatrix(matrixA, ElementOrder.ByRow);
            var t4 = sw.Elapsed.TotalSeconds;
            Console.WriteLine($"Fortran Time to 1st generate matrix {t4:g6} sec");
            sw.Restart();
            var nativeB = new FMatrix(matrixB, ElementOrder.ByRow);
            var t5 = sw.Elapsed.TotalSeconds;
            Console.WriteLine($"Fortran Time to 2nd generate matrix {t5:g6} sec");
            sw.Restart();
            var fortranResult = FMatrix.Product(nativeA, nativeB);
            var t6 = sw.Elapsed.TotalSeconds;
            Console.WriteLine($"Fortran Time to do matrix multiply  {t6:g6} sec");
            sw.Stop();

            // Optional: Output a single value to prevent optimizations
            Console.WriteLine($"Fortran Result(1,1): {fortranResult[1, 1]}");
            Console.WriteLine();

            var maxDelta = ( fortranResult - nativeResult).Max( (x)=>Math.Abs(x));
            Console.WriteLine($"Max Delta between CSharp and Fortran {maxDelta}");

            static double[,] GenerateMatrix(int size)
            {
                double[,] matrix = new double[size, size];
                for (int i = 0; i<size; i++)
                {
                    for (int j = 0; j<size; j++)
                    {
                        matrix[i, j]=rng.NextDouble();
                    }
                }
                return matrix;
            }

            static double[,] MultiplyMatrices(double[,] a, double[,] b, int size)
            {
                double[,] result = new double[size, size];
                for (int i = 0; i<size; i++)
                {
                    for (int j = 0; j<size; j++)
                    {
                        double sum = 0;
                        for (int k = 0; k<size; k++)
                        {
                            sum+=a[i, k]*b[k, j];
                        }
                        result[i, j]=sum;
                    }
                }
                return result;
            }
        }
        static void BenchMatrixSolve()
        {
#if DEBUG
            Console.WriteLine("Benchmark DEBUG Matrix Solver.");
#else
            Console.WriteLine("Benchmark RELEASE Matrix Solver.");
#endif
            Console.WriteLine();
            Console.WriteLine($"CPU#:{Environment.ProcessorCount}, USER:{Environment.UserName}, WIN:{Environment.OSVersion.VersionString}, {( Environment.Is64BitProcess ? "x64" : "x86" )}");


            Console.WriteLine();
            Stopwatch sw = new Stopwatch();
            double t1_μs, t1_s;

            // Burn In
            {
                FMatrix A = FMatrix.RandomMinMax(4, 4, -1.0, 6.0);
                FVector b = FVector.RandomMinMax(4, -3.0, 9.0);
                double sum = 0;
                for (int k = 0; k<20000; k++)
                {
                    FVector g = A.Solve((k+1)*b);
                    sum+=g[1];
                }
            }


            Console.WriteLine($"|=========== SERIAL ============== |");
            Console.WriteLine($"| {"Size",5} {"Time",16} {"Rate",9} ");
            Console.WriteLine($"|--------------------------------- |");
            Console.WriteLine($"| {"[#]",5} {"[s]",16} {"[n^2/μs]",9} ");
            for (int i = -5; i<14; i++)
            {
                int size = i>=0 ? (int)Math.Pow(5, (double)(i+1)/4+1) : i+7;
                FMatrix A = FMatrix.RandomMinMax(size, size, -1.0, 6.0);
                FVector b = FVector.RandomMinMax(size, -3.0, 9.0);

                int repeat = Math.Max(1, 1250000 / (size*size));

                double sum = 0.0;
                FVector x = new FVector(size);
                FVector y = new FVector(size);
                sw.Reset();
                sw.Start();
                for (int iter = 0; iter<repeat; iter++)
                {
                    x=A.Solve(b);
                    sum+=x[1];
                }

                sw.Stop();
                t1_μs=sw.Elapsed.TotalMicroseconds/repeat;
                t1_s=sw.Elapsed.TotalSeconds/repeat;
                sw.Reset();
                Console.WriteLine($"| {size,5} {t1_s,16:g9} {( size*size )/t1_μs,9:g4} ");
            }
        } 
        #endregion
    }
}
