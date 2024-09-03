using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Security;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using System.Globalization;
using System.Drawing;

namespace FortranDriver
{
    public unsafe abstract class NativeMethods
    // NOTE: Fortran methods declared with `DllImport()`. Consider use the newer `LibraryImport()`
    //       delcaration instead. Use a ref to first element instead of passing a 2D array.
    {
        const string libraryName = "FortranDriverDLL";

        #region Vector Operations

        [DllImport(libraryName, EntryPoint = "array_rand_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_rand_v(int size, [In] double minValue, [In] double maxValue, [Out] double[] A);

        [DllImport(libraryName, EntryPoint = "array_elem_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_elem_v(int size, int index, [In] double x, [Out] double[] A);

        [DllImport(libraryName, EntryPoint = "array_add_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_add_v(int size, [In] double[] x, [In] double[] y, [Out] double[] z);

        [DllImport(libraryName, EntryPoint = "array_subtract_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_subtract_v(int size, [In] double[] x, [In] double[] y, [Out] double[] z);

        [DllImport(libraryName, EntryPoint = "array_scale_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_scale_v(int size, double x, [In] double[] y, [Out] double[] z);

        /// <summary>
        /// Fortran DLL call to matrix multiply.
        /// </summary>
        /// <param name="x">The first vector.</param>
        /// <param name="y">The second vector.</param>
        /// <param name="z">The dot product result.</param>
        [DllImport(libraryName, EntryPoint = "array_dot_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_dot_v(int size, [In] double[] x, [In] double[] y, [Out] out double z);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>b=A*x</code>.
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="x">The known vector</param>
        /// <param name="b">The result vector</param>
        [DllImport(libraryName, EntryPoint = "array_product_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_product_mv(int rows, int columns, [In] double[,] A, [In] double[] x, [Out] double[] b);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>b=x*A</code>.
        /// </summary>
        /// <param name="x">The known vector</param>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="b">The result vector</param>
        [DllImport(libraryName, EntryPoint = "array_product_vm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_product_vm(int rows, int columns, [In] double[] x, [In] double[,] A, [Out] double[] b);

        /// <summary>
        /// Fortran DLL call to solve the linear system of equations <code>A*x=b</code> for <paramref name="x"/>.
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="b">The known vector</param>
        /// <param name="x">The unknown vector</param>
        [DllImport(libraryName, EntryPoint = "array_solve_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_solve_mv(int rows, int columns, [In] double[,] A, [In] double[] b, [Out] double[] x);
        #endregion

        #region Matrix Operations

        [DllImport(libraryName, EntryPoint = "array_rand_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_rand_m(int rows, int columns, [In] double minValue, [In] double maxValue, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "array_diag_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_diag_m(int size, [In] double[] x, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "array_scalar_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_scalar_m(int rows, int columns, [In] double x, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "array_add_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_add_m(int rows, int columns, [In] double[,] x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "array_subtract_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_subtract_m(int rows, int columns, [In] double[,] x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "array_scale_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_scale_m(int rows, int columns, double x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "array_det_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_det_m(int size, [In] double[,] A, [Out] out double det);

        [DllImport(libraryName, EntryPoint = "array_tansp_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_tansp_m(int rows, int columns, [In] double[,] A, [Out] double[,] At);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>C=A*B</code>.
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="B">The known matrix</param>
        /// <param name="C">The result matrix</param>
        [DllImport(libraryName, EntryPoint = "array_product_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_product_mm(int rows, int columns, int pages, [In] double[,] A, [In] double[,] B, [Out] double[,] C);

        /// <summary>
        /// Fortran DLL call to solve the linear system of equations <code>A*X=B</code> for <paramref name="X"/>.
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="B">The known matrix. </param>
        /// <param name="X">The unknown matrix.</param>
        [DllImport(libraryName, EntryPoint = "array_solve_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_solve_mm(int rows, int columns, int pages, [In] double[,] A, [In] double[,] B, [Out] double[,] X);

        [DllImport(libraryName, EntryPoint = "array_inverse_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        protected static extern void array_inverse_m(int size, [In] double[,] A, [Out] double[,] A_inv);
        #endregion

    }
    public unsafe class NativeVector : NativeMethods, 
        IFormattable
    {
        public NativeVector(int size)
        {
            this.Size=size;
            this.Data=new double[size];
        }
        internal NativeVector(double[] data)
        {
            this.Size=data.GetLength(0);
            this.Data=data??throw new ArgumentNullException(nameof(data));
        }
        public NativeVector(int size, Func<int, double> initializer)
            : this(size)
        {
            for (int idx = 0; idx < size; idx++)
            {
                Data[idx] = initializer(idx+1);
            }
        }

        public static NativeVector Elemental(int size, int index, double value = 1.0)
        {
            double[] data = new double[size];
            array_elem_v(size, index, value, data);
            return new NativeVector(data);
        }
        public static NativeVector Random(int size, double minValue = 0, double maxValue = 1)
        {
            double[] data = new double[size];
            array_rand_v(size, minValue, maxValue, data);
            return new NativeVector(data);
        }

        public ref double this[int index] => ref Data[index];

        public int Size { get; }
        public int Count { get => Size; }
        internal double[] Data { get; }

        public Span<double> AsSpan()
        {
            fixed (double* ptr = &Data[0])
            {
                return new Span<double>(ptr, Data.Length);
            }
        }

        public double[] ToArray() => Data;

        public static implicit operator double[](NativeVector a) => a.ToArray();
        public static explicit operator NativeVector(double[] a) => new NativeVector(a);

        #region Algebra
        public static double Dot(NativeVector x, NativeVector y)
        {
            int size = Math.Min(x.Size, y.Size);
            array_dot_v(size, x.Data, y.Data, out var result);
            return result;
        }
        public static NativeVector Negate(NativeVector x)
            => Scale(-1, x);
        public static NativeVector Add(NativeVector x, NativeVector y)
        {
            if (x.Size != y.Size)
            {
                throw new ArgumentException($"Expecting {x.Size} elements, found {y.Size}.",nameof(y));
            }
            int n = x.Size;
            double[] data = new double[n];
            array_add_v(n, x.Data, y.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Subtract(NativeVector x, NativeVector y)
        {
            if (x.Size != y.Size)
            {
                throw new ArgumentException($"Expecting {x.Size} elements, found {y.Size}.",nameof(y));
            }
            int n = x.Size;
            double[] data = new double[n];
            array_subtract_v(n, x.Data, y.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Scale(double x, NativeVector A)
        {
            int n = A.Size;
            double[] data = new double[n];
            array_scale_v(n, x, A.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Product(NativeVector x, NativeMatrix A)
        {
            // | A(n,m) | * | x(m) | = | b(n) |
            if (x.Size != A.Rows)
            {
                throw new ArgumentException($"Expecting {x.Size} rows, found {A.Rows}.",nameof(A));
            }
            int n = A.Rows, m = A.Columns;
            double[] data = new double[m];
            array_product_vm(n, m, x.Data, A.Data, data);
            return new NativeVector(data);
        }

        #endregion

        #region Operators
        public static NativeVector operator +(NativeVector a) => a;
        public static NativeVector operator +(NativeVector a, NativeVector b) => Add(a, b);
        public static NativeVector operator -(NativeVector a) => Negate(a);
        public static NativeVector operator -(NativeVector a, NativeVector b) => Subtract(a, b);
        public static NativeVector operator *(double x, NativeVector a) => Scale(x, a);
        public static NativeVector operator *(NativeVector a, double x) => Scale(x, a);
        public static NativeVector operator /(NativeVector a, double x) => Scale(1 / x, a);
        public static double operator *(NativeVector a, NativeVector b) => Dot(a, b);
        public static NativeVector operator *(NativeVector x, NativeMatrix A) => Product(x, A);        
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";

        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            return ToFixedColumnString(formatting, formatProvider);
        }
        public string ToFixedColumnString(string formatting = null, int width = 11)
            => ToFixedColumnString(formatting, null, width);
        public string ToFixedColumnString(string formatting = null, IFormatProvider provider = null, int width = 11)
        {
            if (formatting == null)
            {
                formatting = DefaultFormat;
            }
            if (provider == null)
            {
                provider = CultureInfo.CurrentCulture.NumberFormat;
            }
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < Size; i++)
            {
                sb.Append("|");
                string text = Math.Round(Data[i],12).ToString(formatting, provider).PadLeft(width);
                if (text.Length>width)
                {
                    text = text.Substring(0, width-1) + "…";
                }
                sb.Append($" {text}");
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }
        #endregion

        internal static void TestMethods()
        {

            const int s = 7;

            Console.WriteLine($"Vector Size ={s}");

            var e = Elemental(s, 1);

            Console.WriteLine("Elemental");
            Console.WriteLine($"e =\n{e}");

            var x = Random(s, -1, 6);

            Console.WriteLine("Random");
            Console.WriteLine($"x =\n{x}");

            var n = Negate(x);

            Console.WriteLine("Scaling/Negate");
            Console.WriteLine($"n =\n{n}");

            var y = e + x;
            var z = e - x;

            Console.WriteLine("Addition");
            Console.WriteLine($"y =\n{y}");
            Console.WriteLine("Subtraction");
            Console.WriteLine($"z =\n{z}");

            var d = Dot(y, z);
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
    }
    
    public unsafe class NativeMatrix : NativeMethods,
        IFormattable
    {
        public NativeMatrix(int rows, int columns)
        {
            this.Rows=rows;
            this.Columns=columns;
            this.Data=new double[columns, rows];
        }
        internal NativeMatrix(double[,] data)
        {
            this.Rows=data.GetLength(1);
            this.Columns=data.GetLength(0);
            this.Data=data??throw new ArgumentNullException(nameof(data));
        }
        public NativeMatrix(int rows, int columns, Func<int, int, double> initializer)
            : this(rows, columns)
        {
            if (rows>= columns)
            {
                for (int col_idx = 0; col_idx < columns; col_idx++)
                {
                    for (int row_idx = 0; row_idx < rows; row_idx++)
                    {
                        Data[col_idx, row_idx] = initializer(row_idx+1, col_idx+1);
                    }
                }
            }
            else
            {
                for (int row_idx = 0; row_idx < rows; row_idx++)
                {
                    for (int col_idx = 0; col_idx < columns; col_idx++)
                    {
                        Data[col_idx, row_idx] = initializer(row_idx+1, col_idx+1);
                    }
                }
            }
        }
        public static NativeMatrix Random(int n, int m, double minValue = 0, double maxValue = 1)
        {
            double[,] data = new double[m, n];
            array_rand_m(n, m, minValue, maxValue, data);
            return new NativeMatrix(data);
        }

        public static NativeMatrix Zero(int n) => Zero(n, n);
        public static NativeMatrix Zero(int n, int m) => Scalar(n, m, 0.0);
        public static NativeMatrix Identity(int n) => Identity(n, n);
        public static NativeMatrix Identity(int n, int m) => Scalar(n, m, 1.0);
        public static NativeMatrix Scalar(int n, double value) => Scalar(n, n, value);
        public static NativeMatrix Scalar(int n, int m, double value)
        {
            double[,] data = new double[m, n];
            array_scalar_m(n, m, value, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Diagonal(double[] value)
        {
            int n = value.Length;
            double[,] data = new double[n, n];
            array_diag_m(n, value, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Diagonal(int size, double[] value)
        {
            if (size != value.Length)
            {
                double[] temp = new double[size];
                Array.Copy(value, temp, Math.Min(size, value.Length));
                value = temp;
            }
            double[,] data = new double[size, size];
            array_diag_m(size, value, data);
            return new NativeMatrix(data);
        }

        public ref double this[int rowIndex, int colIndex] => ref Data[colIndex, rowIndex];

        public int Rows { get; }
        public int Columns { get; }
        internal double[,] Data { get; }

        public int Size { get => Math.Min(Rows, Columns); }
        public int Count { get => Rows*Columns; }

        public Span<double> AsSpan()
        {
            fixed (double* ptr = &Data[0, 0])
            {
                return new Span<double>(ptr, Data.Length);
            }
        }

        public double[] ToArray() => AsSpan().ToArray();
        public double[,] ToArray2() => Data;

        public static implicit operator double[,](NativeMatrix a) => a.ToArray2();
        public static explicit operator NativeMatrix(double[,] a) => new NativeMatrix(a);

        public static NativeMatrix Transpose(NativeMatrix A)
        {
            int n = A.Rows, m = A.Columns;
            double[,] data = new double[n, m];
            array_tansp_m(n, m, A.Data, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Negate(NativeMatrix x)
            => Scale(-1, x);

        public static NativeMatrix Add(double x, NativeMatrix y)
            => Scalar(y.Size, x) + y;
        public static NativeMatrix Add(NativeMatrix x, double y)
            => x + Scalar(x.Size, y);
        public static NativeMatrix Add(NativeMatrix x, NativeMatrix y)
        {
            if (x.Rows != y.Rows || x.Columns != y.Columns)
            {
                throw new ArgumentException($"Expecting ({x.Rows},{x.Columns}) elements, found ({y.Rows},{y.Columns}).", nameof(y));
            }
            int n = x.Rows, m = x.Columns;
            double[,] data = new double[m, n];
            array_add_m(n, m, x.Data, y.Data, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Subtract(double x, NativeMatrix y)
            => Scalar(y.Size, x) - y;
        public static NativeMatrix Subtract(NativeMatrix x, double y)
            => x - Scalar(x.Size, y);
        public static NativeMatrix Subtract(NativeMatrix x, NativeMatrix y)
        {
            if (x.Rows != y.Rows || x.Columns != y.Columns)
            {
                throw new ArgumentException($"Expecting ({x.Rows},{x.Columns}) elements, found ({y.Rows},{y.Columns}).", nameof(y));
            }
            int n = x.Rows, m = x.Columns;
            double[,] data = new double[m, n];
            array_subtract_m(n, m, x.Data, y.Data, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Scale(double x, NativeMatrix A)
        {
            int n = A.Rows, m = A.Columns;
            double[,] data = new double[m, n];
            array_scale_m(n, m, x, A.Data, data);
            return new NativeMatrix(data);
        }

        public static NativeVector Product(NativeMatrix A, NativeVector x)
        {
            if (x.Size != A.Rows)
            {
                throw new ArgumentException($"Expecting {x.Size} columns, found {A.Columns}.",nameof(A));
            }
            int n = A.Rows, m = A.Columns;
            double[] data = new double[m];
            array_product_mv(n, m, A.Data, x.Data, data);
            return new NativeVector(data);
        }
        public static NativeMatrix Product(NativeMatrix A, NativeMatrix X)
        {
            // | A(n,m) | * | x(m,k) | = | b(n,k) |
            if (X.Rows != A.Columns)
            {
                throw new ArgumentException($"Expecting {A.Columns} rows, found {X.Rows}.", nameof(X));
            }
            int n = A.Rows, m = A.Columns, k = X.Columns;
            double[,] data = new double[k, n];
            array_product_mm(n, m, k, A.Data, X.Data, data);
            return new NativeMatrix(data);
        }
        public static NativeVector Solve(NativeMatrix A, NativeVector b)
        {
            // | A(n,m) | * | x(m) | = | b(n) |
            if (b.Size != A.Rows)
            {
                throw new ArgumentException($"Expecting {A.Rows} elements, found {b.Size}.",nameof(b));
            }
            int n = A.Rows, m = A.Columns;
            double[] data = new double[m];
            array_solve_mv(n, m, A.Data, b.Data, data);
            return new NativeVector(data);
        }
        public static NativeMatrix Solve(NativeMatrix A, NativeMatrix B)
        {
            // | A(n,m) | * | x(m,k) | = | b(n,k) |
            if (B.Rows != A.Rows)
            {
                throw new ArgumentException($"Expecting {A.Rows} elements, found {B.Rows}.", nameof(B));
            }
            int n = A.Rows, m = A.Columns, k = B.Columns;
            double[,] data = new double[k, m];
            array_solve_mm(n, m, k, A.Data, B.Data, data);
            return new NativeMatrix(data);
        }

        #region Algebra
        public static double Determinant(NativeMatrix a)
        {
            int n = a.Size;
            array_det_m(n, a.Data, out double det);
            return det;
        }
        public static NativeMatrix Inverse(NativeMatrix a)
        {
            int n = a.Size;
            double[,] data = new double[n, n];
            array_inverse_m(n, a.Data, data);
            return new NativeMatrix(data);
        }
        #endregion

        #region Operators
        public static NativeMatrix operator +(NativeMatrix a) => a;
        public static NativeMatrix operator -(NativeMatrix a) => Negate(a);
        public static NativeMatrix operator +(double a, NativeMatrix b) => Add(a, b);
        public static NativeMatrix operator -(double a, NativeMatrix b) => Subtract(a, b);
        public static NativeMatrix operator +(NativeMatrix a, double b) => Add(a, b);
        public static NativeMatrix operator -(NativeMatrix a, double b) => Subtract(a, b);
        public static NativeMatrix operator +(NativeMatrix a, NativeMatrix b) => Add(a, b);
        public static NativeMatrix operator -(NativeMatrix a, NativeMatrix b) => Subtract(a, b);
        public static NativeMatrix operator *(double factor, NativeMatrix a) => Scale(factor, a);
        public static NativeMatrix operator *(NativeMatrix a, double factor) => Scale(factor, a);
        public static NativeVector operator *(NativeMatrix a, NativeVector v) => Product(a, v);
        public static NativeMatrix operator *(NativeMatrix a, NativeMatrix b) => Product(a, b);
        public static NativeMatrix operator /(NativeMatrix a, double divisor) => Scale(1 / divisor, a);
        public static NativeMatrix operator ~(NativeMatrix a) => Transpose(a);
        public static NativeVector operator /(NativeVector x, NativeMatrix A) => Solve(A, x);
        public static NativeMatrix operator /(NativeMatrix X, NativeMatrix A) => Solve(A, X);
        public static NativeMatrix operator !(NativeMatrix a) => Inverse(a);
        #endregion


        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";

        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            return ToFixedColumnString(formatting, formatProvider);
        }
        public string ToFixedColumnString(string formatting = null, int width = 11)
            => ToFixedColumnString(formatting, null, width);
        public string ToFixedColumnString(string formatting = null, IFormatProvider provider = null, int width = 11)
        {
            if (formatting == null)
            {
                formatting = DefaultFormat;
            }
            if (provider == null)
            {
                provider = CultureInfo.CurrentCulture.NumberFormat;
            }
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < Rows; i++)
            {
                sb.Append("|");
                for (int j = 0; j < Columns; j++)
                {
                    string text = Math.Round(Data[j, i], 12).ToString(formatting, provider).PadLeft(width);
                    if (text.Length>width)
                    {
                        text = text.Substring(0, width-1) + "…";
                    }
                    sb.Append($" {text}");
                }
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }
        #endregion

        internal static void TestMethods()
        {

            const int s = 7, t = 3;

            var r = NativeVector.Random(s, -1, 6);

            var D = Diagonal(r.ToArray());
            var S = Scalar(s, s, 2.0);
            var R = Random(s, s, -1, 6);
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

            var d = Determinant(R);
            Console.WriteLine("Determinant");
            Console.WriteLine($"d = \n{d}");

            var A_tr = Transpose(A);
            Console.WriteLine("Transpose");
            Console.WriteLine($"A_tr = \n{A_tr}");

            var A_inv = Inverse(A);
            Console.WriteLine("Inverse");
            Console.WriteLine($"A_inv = \n{A_inv}");

            var I = A_inv * A;

            Console.WriteLine("Check Identity");
            Console.WriteLine($"A_inv*A=\n{I}");
            Console.WriteLine($"A_inv*A-1=\n{I-1}");

            var Y = Random(s, t, -1, 6);
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
    }
}
