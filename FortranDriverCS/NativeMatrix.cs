using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Linq.Expressions;

namespace FortranDriver
{
    public unsafe class NativeMatrix :
        System.Collections.ICollection,
        ICollection<double>,
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
        public static NativeMatrix FromRows(int rows, int columns, params double[] values)
        {
            double[,] data = new double[rows, columns];
            FortranMethods.array_reshape_vm(rows*columns, values, columns, rows, data);
            double[,] temp = new double[columns, rows];
            FortranMethods.array_tansp_m(columns, rows, data, temp);
            return new NativeMatrix(temp);
        }
        public static NativeMatrix FromColumns(int rows, int columns, params double[] values)
        {
            double[,] data = new double[columns, rows];
            FortranMethods.array_reshape_vm(rows*columns, values, rows, columns, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix RandomMinMax(int n, int m, double minValue = 0, double maxValue = 1)
        {
            double[,] data = new double[m, n];
            FortranMethods.array_random_m(n, m, minValue, maxValue, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix RandomUniform(int n, int m, ref int seed)
        {
            double[,] data = new double[m, n];
            FortranMethods.array_uniform_m(n, m, ref seed, data);
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
            FortranMethods.array_scalar_m(n, m, value, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Diagonal(double[] value)
        {
            int n = value.Length;
            double[,] data = new double[n, n];
            FortranMethods.array_diag_m(n, value, data);
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
            FortranMethods.array_diag_m(size, value, data);
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
                return Slice(i1, i2, j1, j2);
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
                return Slice(i1, i2, j1, j2);
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
                return Slice(i1, i2, j1, j2);
            }
        }

        public int Rows { get; }
        public int Columns { get; }
        internal double[,] Data { get; }

        public int Size { get => Math.Min(Rows, Columns); }
        public double[,] ToArray2() => Data;

        public static implicit operator double[,](NativeMatrix a) => a.ToArray2();
        public static explicit operator NativeMatrix(double[,] a) => new NativeMatrix(a);

        #region Algebra

        public static NativeMatrix Round(NativeMatrix A, int digits)
        {
            double[,] data = new double[A.Columns, A.Rows];
            FortranMethods.array_round_m(A.Rows, A.Columns, A.Data, digits, data);
            return new NativeMatrix(data);
        }

        public NativeMatrix ReShape(int newRows, int newColumns)
        {
            int n = Rows, m = Columns, k = newRows, l = newColumns;
            double[,] data = new double[l, k];
            FortranMethods.array_reshape_mm(n, m, Data, k, l, data);
            return new NativeMatrix(data);
        }
        public NativeVector ReShape(int newSize)
        {
            int n = Rows, m = Columns, k = newSize;
            double[] data = new double[k];
            FortranMethods.array_reshape_mv(n, m, Data, k, data);
            return new NativeVector(data);
        }
        public NativeMatrix Slice(int startRow, int endRow, int startColumn, int endColumn)
        {
            double[,] data = new double[endColumn-startColumn+1, endRow-startRow+1];
            FortranMethods.array_slice_m(Rows, Columns, Data, startRow, endRow, startColumn, endColumn, data);
            return new NativeMatrix(data);
        }
        public NativeVector GetRow(int row)
            => GetRow(row, 1, Columns);
        public NativeVector GetRow(int row, int startColumn, int endColumn)
        {
            int size = endColumn-startColumn+1;
            double[,] matrix = new double[size, 1];
            FortranMethods.array_slice_m(Rows, Columns, Data, row, row, startColumn, endColumn, matrix);
            double[] data = new double[size];
            FortranMethods.array_reshape_mv(size, 1, matrix, size, data);
            return new NativeVector(data);
        }
        public NativeVector GetColumn(int row)
            => GetColumn(row, 1, Columns);
        public NativeVector GetColumn(int row, int startRow, int endRow)
        {
            int size = endRow-startRow+1;
            double[,] matrix = new double[1, size];
            FortranMethods.array_slice_m(Rows, Columns, Data, startRow, endRow, row, row, matrix);
            double[] data = new double[size];
            FortranMethods.array_reshape_mv(1, size, matrix, size, data);
            return new NativeVector(data);
        }
        public static NativeMatrix Transpose(NativeMatrix A)
        {
            int n = A.Rows, m = A.Columns;
            double[,] data = new double[n, m];
            FortranMethods.array_tansp_m(n, m, A.Data, data);
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
            FortranMethods.array_add_m(n, m, x.Data, y.Data, data);
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
            FortranMethods.array_subtract_m(n, m, x.Data, y.Data, data);
            return new NativeMatrix(data);
        }
        public static NativeMatrix Scale(double x, NativeMatrix A)
        {
            int n = A.Rows, m = A.Columns;
            double[,] data = new double[m, n];
            FortranMethods.array_scale_m(n, m, x, A.Data, data);
            return new NativeMatrix(data);
        }

        public static NativeVector Product(NativeVector x, NativeMatrix A)
        {
            if (x.Size != A.Columns)
            {
                throw new ArgumentException($"Expecting {x.Size} columns, found {A.Columns}.", nameof(A));
            }
            int n = A.Rows, m = A.Columns;
            double[] data = new double[m];
            FortranMethods.array_product_vm(n, m, x.Data, A.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Product(NativeMatrix A, NativeVector x)
        {
            if (x.Size != A.Rows)
            {
                throw new ArgumentException($"Expecting {x.Size} columns, found {A.Columns}.", nameof(A));
            }
            int n = A.Rows, m = A.Columns;
            double[] data = new double[m];
            FortranMethods.array_product_mv(n, m, A.Data, x.Data, data);
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
            FortranMethods.array_product_mm(n, m, k, A.Data, X.Data, data);
            return new NativeMatrix(data);
        }

        public double Determinant()
        {
            int n = Size;
            FortranMethods.array_det_m(n, Data, out double det);
            return det;
        }
        public NativeVector Solve(NativeVector b)
        {
            // | A(n,n) | * | x(n) | = | b(n) |
            if (b.Size != Rows)
            {
                throw new ArgumentException($"Expecting {Rows} elements, found {b.Size}.", nameof(b));
            }
            int n = Rows, m = Columns;
            double[] data = new double[m];
            FortranMethods.array_solve_mv(n, Data, b.Data, data);
            return new NativeVector(data);
        }
        public NativeMatrix Solve(NativeMatrix B)
        {
            // | A(n,n) | * | x(n,k) | = | b(n,k) |
            if (B.Rows != Rows)
            {
                throw new ArgumentException($"Expecting {Rows} elements, found {B.Rows}.", nameof(B));
            }
            int n = Rows, m = Columns, k = B.Columns;
            double[,] data = new double[k, m];
            FortranMethods.array_solve_mm(n, k, Data, B.Data, data);
            return new NativeMatrix(data);
        }
        public NativeVector BlockSolve(NativeVector b)
        {
            // | A(n,n) | * | x(n) | = | b(n) |
            if (b.Size != Rows)
            {
                throw new ArgumentException($"Expecting {Rows} elements, found {b.Size}.", nameof(b));
            }
            int n = Rows, m = Columns;
            double[] data = new double[m];
            FortranMethods.array_block_solve_mv(n, Data, b.Data, data);
            return new NativeVector(data);
        }
        public NativeMatrix BlockSolve(NativeMatrix B)
        {
            // | A(n,n) | * | x(n,k) | = | b(n,k) |
            if (B.Rows != Rows)
            {
                throw new ArgumentException($"Expecting {Rows} elements, found {B.Rows}.", nameof(B));
            }
            int n = Rows, m = Columns, k = B.Columns;
            double[,] data = new double[k, m];
            FortranMethods.array_block_solve_mm(n, k, Data, B.Data, data);
            return new NativeMatrix(data);
        }

        public NativeMatrix Inverse()
        {
            int n = Size;
            double[,] data = new double[n, n];
            FortranMethods.array_inverse_m(n, Data, data);
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
        public static NativeMatrix operator /(NativeMatrix X, NativeMatrix A) => A.Solve(X);
        public static NativeMatrix operator !(NativeMatrix a) => a.Inverse();
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";

        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            const int width = 11;

            //double[,] data = new double[Columns, Rows];
            //FortranMethods.array_round_m(Rows, Columns, Data, 11, data);

            StringBuilder sb = new StringBuilder();
            int n = Data.GetLength(1), m = Data.GetLength(0);
            string[][] data = new string[m][];
            int[] maxWidth = new int[m];
            for (int j = 0; j < m; j++)
            {
                data[j] = new string[n];
                for (int i = 0; i < n; i++)
                {
                    double x = Math.Round(Data[j, i], 11);
                    data[j][i] = x.ToString(formatting, formatProvider);
                }
                maxWidth[j] = data[j].Max((s) => s.Length);
            }

            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                for (int j = 0; j < m; j++)
                {
                    string text = data[j][i];
                    text  = text.PadLeft(maxWidth[j]);
                    sb.Append($" {text}");
                }
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }
        #endregion

        #region Collections
        public int Count { get => Rows*Columns; }

        public Span<double> AsSpan()
        {
            fixed (double* ptr = &Data[0, 0])
            {
                return new Span<double>(ptr, Data.Length);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public double[] ToArray() => AsSpan().ToArray();

        public bool Contains(double item) => IndexOf(item)>=0;
        public int IndexOf(double item) => Array.IndexOf(Data, item);
        public IEnumerator<double> GetEnumerator()
        {
            for (int j = 0; j < Columns; j++)
            {
                for (int i = 0; i < Rows; i++)
                {
                    yield return Data[j, i];
                }
            }
        }
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
            => GetEnumerator();
        bool ICollection<double>.IsReadOnly => true;
        void ICollection<double>.Add(double item) => throw new NotSupportedException();
        void ICollection<double>.Clear() => throw new NotSupportedException();
        bool ICollection<double>.Remove(double item) => throw new NotSupportedException();
        bool System.Collections.ICollection.IsSynchronized => false;
        object System.Collections.ICollection.SyncRoot => null;
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        void System.Collections.ICollection.CopyTo(Array array, int index)
        {
            CopyTo(array as double[], index);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void CopyTo(double[] array, int index)
        {
            array = new double[Data.Length];
            Buffer.BlockCopy(Data, 0 , array, sizeof(double) * index, Buffer.ByteLength(Data));
        }

        #endregion


    }
}
