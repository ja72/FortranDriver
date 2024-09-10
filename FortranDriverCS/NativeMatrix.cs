using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Security;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using System.Globalization;
using System.Drawing;
using System.ComponentModel;
using System.Runtime.InteropServices;
using static System.Runtime.InteropServices.JavaScript.JSType;

namespace FortranDriver
{
    public unsafe class NativeMatrix : 
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
            NativeMatrixMethods.array_rand_m(n, m, minValue, maxValue, data);
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
            NativeMatrixMethods.array_scalar_m(n, m, value, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Diagonal(double[] value)
        {
            int n = value.Length;
            double[,] data = new double[n, n];
            NativeMatrixMethods.array_diag_m(n, value, data);
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
            NativeMatrixMethods.array_diag_m(size, value, data);
            return new NativeMatrix(data);
        }

        public ref double this[Index row, Index column]
        {
            get
            {
                int i = row.GetOffset(Rows) + (row.IsFromEnd ? 1 : 0);
                int j = column.GetOffset(Columns) + (column.IsFromEnd ? 1 : 0);
                return ref Data[j-1, i-1];
            }
        }
        public NativeMatrix this[Index row, Range columns]
        {
            get
            {
                int i1 = row.GetOffset(Rows) + (row.IsFromEnd ? 1 : 0);
                (int j1, int m1) = columns.GetOffsetAndLength(Rows);
                j1 += columns.Start.IsFromEnd ? 1 : 0;
                int i2 = i1, j2 = j1 + m1;
                return Slice(this, i1, i2, j1, j2);
            }
        }
        public NativeMatrix this[Range rows, Index column]
        {
            get
            {
                (int i1, int n1) = rows.GetOffsetAndLength(Rows);
                i1 += rows.Start.IsFromEnd ? 1 : 0;
                int j1 = column.GetOffset(Columns) + (column.IsFromEnd ? 1 : 0);
                int i2 = i1 + n1, j2 = j1;
                return Slice(this, i1, i2, j1, j2);
            }
        }
        public NativeMatrix this[Range rows, Range columns]
        {
            get
            {
                (int i1, int n1) = rows.GetOffsetAndLength(Rows);
                i1 += rows.Start.IsFromEnd ? 1 : 0;
                (int j1, int m1) = columns.GetOffsetAndLength(Columns);
                j1 += columns.Start.IsFromEnd ? 1 : 0;
                int i2 = i1 + n1, j2 = j1 + m1;
                return Slice(this, i1, i2, j1, j2);
            }
        }

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
            NativeMatrixMethods.array_tansp_m(n, m, A.Data, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Reshape(NativeMatrix A, int newRows, int newColumns)
        {
            int n = A.Rows, m = A.Columns, k = newRows, l = newColumns;
            double[,] data = new double[l, k];
            NativeMatrixMethods.array_reshape_mm(n, m, A.Data, k, l, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Slice(NativeMatrix A, int startRow, int endRow, int startColumn, int endColumn)
        {
            double[,] data = new double[endColumn-startColumn+1, endRow-startRow+1];
            NativeMatrixMethods.array_slice_m(A.Rows, A.Columns, A, startRow, endRow, startColumn, endColumn, data);
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
            NativeMatrixMethods.array_add_m(n, m, x.Data, y.Data, data);
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
            NativeMatrixMethods.array_subtract_m(n, m, x.Data, y.Data, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Scale(double x, NativeMatrix A)
        {
            int n = A.Rows, m = A.Columns;
            double[,] data = new double[m, n];
            NativeMatrixMethods.array_scale_m(n, m, x, A.Data, data);
            return new NativeMatrix(data);
        }

        public static NativeVector Product(NativeMatrix A, NativeVector x)
        {
            if (x.Size != A.Rows)
            {
                throw new ArgumentException($"Expecting {x.Size} columns, found {A.Columns}.", nameof(A));
            }
            int n = A.Rows, m = A.Columns;
            double[] data = new double[m];
            NativeMatrixMethods.array_product_mv(n, m, A.Data, x.Data, data);
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
            NativeMatrixMethods.array_product_mm(n, m, k, A.Data, X.Data, data);
            return new NativeMatrix(data);
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
            NativeMatrixMethods.array_solve_mm(n, m, k, A.Data, B.Data, data);
            return new NativeMatrix(data);
        }

        #region Algebra
        public static double Determinant(NativeMatrix a)
        {
            int n = a.Size;
            NativeMatrixMethods.array_det_m(n, a.Data, out double det);
            return det;
        }
        public static NativeMatrix Inverse(NativeMatrix a)
        {
            int n = a.Size;
            double[,] data = new double[n, n];
            NativeMatrixMethods.array_inverse_m(n, a.Data, data);
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
        public static NativeMatrix operator /(NativeMatrix X, NativeMatrix A) => Solve(A, X);
        public static NativeMatrix operator !(NativeMatrix a) => Inverse(a);
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";

        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            const int width = 11;
            StringBuilder sb = new StringBuilder();
            int n = Data.GetLength(1), m = Data.GetLength(0);
            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                for (int j = 0; j < m; j++)
                {
                    var obj = Data[j, i];
                    string text = obj.ToString(formatting, formatProvider);
                    text  = text.PadLeft(width);
                    if (text.Length>width)
                    {
                        text = $"{text.Substring(0, width-1)}…";
                    }
                    sb.Append($" {text}");
                }
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }
        #endregion

        #region Fortran Methods
        // NOTE: Fortran methods declared with `DllImport()`. Consider use the newer `LibraryImport()`
        //       delcaration instead. Use a ref to first element instead of passing a 2D array.

        internal static class NativeMatrixMethods
        {
            const string libraryName = "FortranDriverDLL";

            [DllImport(libraryName, EntryPoint = "array_rand_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_rand_m(int rows, int columns, [In] double minValue, [In] double maxValue, [Out] double[,] A);

            [DllImport(libraryName, EntryPoint = "array_diag_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_diag_m(int size, [In] double[] x, [Out] double[,] A);

            [DllImport(libraryName, EntryPoint = "array_scalar_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_scalar_m(int rows, int columns, [In] double x, [Out] double[,] A);

            [DllImport(libraryName, EntryPoint = "array_norm_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_norm_m(int rows, int columns, [In] double[,] A, ref double s);

            [DllImport(libraryName, EntryPoint = "array_add_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_add_m(int rows, int columns, [In] double[,] x, [In] double[,] y, [Out] double[,] z);

            [DllImport(libraryName, EntryPoint = "array_subtract_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_subtract_m(int rows, int columns, [In] double[,] x, [In] double[,] y, [Out] double[,] z);

            [DllImport(libraryName, EntryPoint = "array_scale_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_scale_m(int rows, int columns, double x, [In] double[,] y, [Out] double[,] z);

            [DllImport(libraryName, EntryPoint = "array_det_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_det_m(int size, [In] double[,] A, [Out] out double det);

            [DllImport(libraryName, EntryPoint = "array_tansp_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_tansp_m(int rows, int columns, [In] double[,] A, [Out] double[,] At);

            [DllImport(libraryName, EntryPoint = "array_reshape_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_reshape_mm(int rows, int columns, [In] double[,] A, int new_rows, int new_columns, [Out] double[,] B);

            [DllImport(libraryName, EntryPoint = "array_slice_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_slice_m(int rows, int columns, [In] double[,] A, int start_row, int end_row, int start_column, int end_column, [Out] double[,] B);


            /// <summary>
            /// Fortran DLL call to matrix multiply <code>b=A*x</code>.
            /// </summary>
            /// <param name="A">The coefficient matrix.</param>
            /// <param name="x">The known vector</param>
            /// <param name="b">The result vector</param>
            [DllImport(libraryName, EntryPoint = "array_product_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_product_mv(int rows, int columns, [In] double[,] A, [In] double[] x, [Out] double[] b);

            /// <summary>
            /// Fortran DLL call to matrix multiply <code>C=A*B</code>.
            /// </summary>
            /// <param name="A">The coefficient matrix.</param>
            /// <param name="B">The known matrix</param>
            /// <param name="C">The result matrix</param>
            [DllImport(libraryName, EntryPoint = "array_product_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_product_mm(int rows, int columns, int pages, [In] double[,] A, [In] double[,] B, [Out] double[,] C);

            /// <summary>
            /// Fortran DLL call to solve the linear system of equations <code>A*X=B</code> for <paramref name="X"/>.
            /// </summary>
            /// <param name="A">The coefficient matrix.</param>
            /// <param name="B">The known matrix. </param>
            /// <param name="X">The unknown matrix.</param>
            [DllImport(libraryName, EntryPoint = "array_solve_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_solve_mm(int rows, int columns, int pages, [In] double[,] A, [In] double[,] B, [Out] double[,] X);

            [DllImport(libraryName, EntryPoint = "array_inverse_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_inverse_m(int size, [In] double[,] A, [Out] double[,] A_inv);

        }

        #endregion

    }
}
