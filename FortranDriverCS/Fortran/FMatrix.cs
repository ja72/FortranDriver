using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Linq.Expressions;

namespace JA.Fortran
{
    /// <summary>
    /// Specify element ordering. <see cref="ByRow"/> = 0 (default for CLR), <see cref="ByColumn"/> = 1 (default for Fortran)
    /// </summary>
    public enum ElementOrder
    {
        /// <summary>
        /// Order elements row by row
        /// </summary>
        ByRow = 0,
        /// <summary>
        /// Order elements column by column
        /// </summary>
        ByColumn = 1,
    }
    public unsafe partial class FMatrix :
        System.Collections.ICollection,
        ICollection<double>,
        IFormattable
    {
        public FMatrix(int rows, int columns)
        {
            Rows = rows;
            Columns = columns;
            Data = new double[columns, rows];
            Order = ElementOrder.ByColumn;
        }
        internal FMatrix(double[,] data, ElementOrder order = ElementOrder.ByColumn)
        {
            switch (order)
            {
                case ElementOrder.ByRow:
                {
                    Rows=data.GetLength(0);
                    Columns=data.GetLength(1);
                    var temp = new double[Columns,Rows];
                    FortranMethods.call_transpose_array_m(Rows, Columns, data, temp);
                    Data=temp;
                    Order=ElementOrder.ByColumn;
                }
                break;
                case ElementOrder.ByColumn:
                {
                    Rows = data.GetLength(1);
                    Columns = data.GetLength(0);
                    Data=data??throw new ArgumentNullException(nameof(data));
                    Order = ElementOrder.ByColumn;
                }
                break;
                default:
                throw new NotSupportedException(order.ToString());
            }
        }
        FMatrix(int rows, int columns, double[] values, ElementOrder order = ElementOrder.ByRow)
            : this(rows, columns)
        {
            double[,] data = Data;
            FortranMethods.call_fill_array_m(rows, columns, values, order, data);
        }
        public FMatrix(int rows, int columns, Func<int, int, double> initializer)
            : this(rows, columns)
        {
            double[,] data = Data;
            if (rows >= columns)
            {
                for (int col_idx = 0; col_idx < columns; col_idx++)
                {
                    for (int row_idx = 0; row_idx < rows; row_idx++)
                    {
                        data[col_idx, row_idx] = initializer(row_idx + 1, col_idx + 1);
                    }
                }
            }
            else
            {
                for (int row_idx = 0; row_idx < rows; row_idx++)
                {
                    for (int col_idx = 0; col_idx < columns; col_idx++)
                    {
                        data[col_idx, row_idx] = initializer(row_idx + 1, col_idx + 1);
                    }
                }
            }
        }
        public static FMatrix FromRows(int rows, int columns, params double[] values)
        {
            return new FMatrix(rows, columns, values, ElementOrder.ByRow);
            //double[,] data = new double[rows, columns];
            //FortranMethods.call_reshape_array_vm(rows*columns, values, columns, rows, data);
            //double[,] temp = new double[columns, rows];
            //FortranMethods.call_transpose_array_m(columns, rows, data, temp);
            //return new NativeMatrix(temp);
        }
        public static FMatrix FromColumns(int rows, int columns, params double[] values)
        {
            return new FMatrix(rows, columns, values, ElementOrder.ByColumn);
            //double[,] data = new double[columns, rows];
            //FortranMethods.call_reshape_array_vm(rows*columns, values, rows, columns, data);
            //return new NativeMatrix(data);
        }
        public static FMatrix RandomMinMax(int n, int m, double minValue = 0, double maxValue = 1)
        {
            double[,] data = new double[m, n];
            FortranMethods.call_random_array_m(n, m, minValue, maxValue, data);
            return new FMatrix(data);
        }
        public static FMatrix RandomUniform(int n, int m, ref int seed)
        {
            double[,] data = new double[m, n];
            FortranMethods.call_uniform_array_m(n, m, ref seed, data);
            return new FMatrix(data);
        }

        public static FMatrix Zero(int n) => Zero(n, n);
        public static FMatrix Zero(int n, int m) => Scalar(n, m, 0.0);
        public static FMatrix Identity(int n) => Identity(n, n);
        public static FMatrix Identity(int n, int m) => Scalar(n, m, 1.0);
        public static FMatrix Scalar(int n, double value) => Scalar(n, n, value);
        public static FMatrix Scalar(int n, int m, double value)
        {
            double[,] data = new double[m, n];
            FortranMethods.call_array_scalar_m(n, m, value, data);
            return new FMatrix(data);
        }
        public static FMatrix Diagonal(double[] value)
        {
            int n = value.Length;
            double[,] data = new double[n, n];
            FortranMethods.call_array_diag_m(n, value, data);
            return new FMatrix(data);
        }
        public static FMatrix Diagonal(int size, double[] value)
        {
            if (size != value.Length)
            {
                double[] temp = new double[size];
                Array.Copy(value, temp, Math.Min(size, value.Length));
                value = temp;
            }
            double[,] data = new double[size, size];
            FortranMethods.call_array_diag_m(size, value, data);
            return new FMatrix(data);
        }

        public ElementOrder Order { get; }

        public ref double this[Index row, Index column]
        {
            get
            {
                int i = row.GetOffset(Rows) + (row.IsFromEnd ? 1 : 0);
                int j = column.GetOffset(Columns) + (column.IsFromEnd ? 1 : 0);
                return ref Data[j - 1, i - 1];
            }
        }
        public FMatrix this[Index row, Range columns]
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
        public FMatrix this[Range rows, Index column]
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
        public FMatrix this[Range rows, Range columns]
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

        public static implicit operator double[,](FMatrix a) => a.ToArray2();
        public static explicit operator FMatrix(double[,] a) => new FMatrix(a);

        #region Algebra

        public static FMatrix Round(FMatrix A, int digits)
        {
            double[,] data = new double[A.Columns, A.Rows];
            FortranMethods.call_round_array_m(A.Rows, A.Columns, A.Data, digits, data);
            return new FMatrix(data);
        }

        public FMatrix ReShape(int newRows, int newColumns)
        {
            int n = Rows, m = Columns, k = newRows, l = newColumns;
            double[,] data = new double[l, k];
            FortranMethods.call_reshape_array_mm(n, m, Data, k, l, data);
            return new FMatrix(data);
        }
        public FVector ReShape(int newSize)
        {
            int n = Rows, m = Columns, k = newSize;
            double[] data = new double[k];
            FortranMethods.call_reshape_array_mv(n, m, Data, k, data);
            return new FVector(data);
        }
        public FMatrix Slice(int startRow, int endRow, int startColumn, int endColumn)
        {
            double[,] data = new double[endColumn - startColumn + 1, endRow - startRow + 1];
            FortranMethods.call_slice_array_m(Rows, Columns, Data, startRow, endRow, startColumn, endColumn, data);
            return new FMatrix(data);
        }
        public FVector GetRow(int row)
            => GetRow(row, 1, Columns);
        public FVector GetRow(int row, int startColumn, int endColumn)
        {
            int size = endColumn - startColumn + 1;
            double[,] matrix = new double[size, 1];
            FortranMethods.call_slice_array_m(Rows, Columns, Data, row, row, startColumn, endColumn, matrix);
            double[] data = new double[size];
            FortranMethods.call_reshape_array_mv(size, 1, matrix, size, data);
            return new FVector(data);
        }
        public FVector GetColumn(int row)
            => GetColumn(row, 1, Columns);
        public FVector GetColumn(int row, int startRow, int endRow)
        {
            int size = endRow - startRow + 1;
            double[,] matrix = new double[1, size];
            FortranMethods.call_slice_array_m(Rows, Columns, Data, startRow, endRow, row, row, matrix);
            double[] data = new double[size];
            FortranMethods.call_reshape_array_mv(1, size, matrix, size, data);
            return new FVector(data);
        }
        public static FMatrix Transpose(FMatrix A)
        {
            int n = A.Rows, m = A.Columns;
            double[,] data = new double[n, m];
            FortranMethods.call_transpose_array_m(n, m, A.Data, data);
            return new FMatrix(data);
        }
        public static FMatrix Negate(FMatrix x)
            => Scale(-1, x);

        public static FMatrix Add(double x, FMatrix y)
            => Scalar(y.Size, x) + y;
        public static FMatrix Add(FMatrix x, double y)
            => x + Scalar(x.Size, y);
        public static FMatrix Add(FMatrix x, FMatrix y)
        {
            if (x.Rows != y.Rows || x.Columns != y.Columns)
            {
                throw new ArgumentException($"Expecting ({x.Rows},{x.Columns}) elements, found ({y.Rows},{y.Columns}).", nameof(y));
            }
            int n = x.Rows, m = x.Columns;
            double[,] data = new double[m, n];
            FortranMethods.call_add_array_m(n, m, x.Data, y.Data, data);
            return new FMatrix(data);
        }
        public static FMatrix Subtract(double x, FMatrix y)
            => Scalar(y.Size, x) - y;
        public static FMatrix Subtract(FMatrix x, double y)
            => x - Scalar(x.Size, y);
        public static FMatrix Subtract(FMatrix x, FMatrix y)
        {
            if (x.Rows != y.Rows || x.Columns != y.Columns)
            {
                throw new ArgumentException($"Expecting ({x.Rows},{x.Columns}) elements, found ({y.Rows},{y.Columns}).", nameof(y));
            }
            int n = x.Rows, m = x.Columns;
            double[,] data = new double[m, n];
            FortranMethods.call_sub_array_m(n, m, x.Data, y.Data, data);
            return new FMatrix(data);
        }
        public static FMatrix Scale(double x, FMatrix A)
        {
            int n = A.Rows, m = A.Columns;
            double[,] data = new double[m, n];
            FortranMethods.call_scale_array_m(n, m, x, A.Data, data);
            return new FMatrix(data);
        }

        public static FVector Product(FVector x, FMatrix A)
        {
            if (x.Size != A.Columns)
            {
                throw new ArgumentException($"Expecting {x.Size} columns, found {A.Columns}.", nameof(A));
            }
            int n = A.Rows, m = A.Columns;
            double[] b_data = new double[m];
            FortranMethods.call_mul_array_vm(n, m, x.Data, A.Data, b_data);
            return new FVector(b_data);
        }
        public static FVector Product(FMatrix A, FVector x)
        {
            if (x.Size != A.Rows)
            {
                throw new ArgumentException($"Expecting {x.Size} columns, found {A.Columns}.", nameof(A));
            }
            int n = A.Rows, m = A.Columns;
            double[] b_data = new double[m];
            FortranMethods.call_mul_array_mv(n, m, A.Data, x.Data, b_data);
            return new FVector(b_data);
        }
        public static FMatrix Product(FMatrix A, FMatrix X)
        {
            // | A(n,m) | * | x(m,k) | = | b(n,k) |
            int n = A.Rows, m = A.Columns, k = X.Columns;
            if (X.Rows != m)
            {
                throw new ArgumentException($"Expecting {A.Columns} rows, found {X.Rows}.", nameof(X));
            }
            double[,] b_data = new double[k, n];
            FortranMethods.call_mul_array_mm(n, m, k, A.Data, X.Data, b_data);
            return new FMatrix(b_data);
        }

        public double Determinant()
        {
            int n = Size;
            FortranMethods.call_determinant_array_m(n, Data, out double det);
            return det;
        }
        public FVector Solve(FVector b)
        {
            // | A(n,n) | * | x(n) | = | b(n) |
            if (b.Size != Rows)
            {
                throw new ArgumentException($"Expecting {Rows} elements, found {b.Size}.", nameof(b));
            }
            int n = Rows, m = Columns;
            double[] data = new double[m];
            FortranMethods.call_solve_array_mv(n, Data, b.Data, data);
            return new FVector(data);
        }
        public FMatrix Solve(FMatrix B)
        {
            // | A(n,n) | * | x(n,k) | = | b(n,k) |
            int n = Rows, k = B.Columns;
            if (B.Rows != n)
            {
                throw new ArgumentException($"Expecting {n} rows, found {B.Rows}.", nameof(B));
            }
            double[,] data = new double[k, n];
            FortranMethods.call_solve_array_mm(n, k, Data, B.Data, data);
            return new FMatrix(data);
        }
        public FMatrix Inverse()
        {
            int n = Size;
            double[,] data = new double[n, n];
            FortranMethods.call_inverse_array_m(n, Data, data);
            return new FMatrix(data);
        }
        #endregion

        #region Operators
        public static FMatrix operator +(FMatrix a) => a;
        public static FMatrix operator -(FMatrix a) => Negate(a);
        public static FMatrix operator +(double a, FMatrix b) => Add(a, b);
        public static FMatrix operator -(double a, FMatrix b) => Subtract(a, b);
        public static FMatrix operator +(FMatrix a, double b) => Add(a, b);
        public static FMatrix operator -(FMatrix a, double b) => Subtract(a, b);
        public static FMatrix operator +(FMatrix a, FMatrix b) => Add(a, b);
        public static FMatrix operator -(FMatrix a, FMatrix b) => Subtract(a, b);
        public static FMatrix operator *(double factor, FMatrix a) => Scale(factor, a);
        public static FMatrix operator *(FMatrix a, double factor) => Scale(factor, a);
        public static FVector operator *(FMatrix a, FVector v) => Product(a, v);
        public static FMatrix operator *(FMatrix a, FMatrix b) => Product(a, b);
        public static FMatrix operator /(FMatrix a, double divisor) => Scale(1 / divisor, a);
        public static FMatrix operator ~(FMatrix a) => Transpose(a);
        public static FMatrix operator /(FMatrix X, FMatrix A) => A.Solve(X);
        public static FMatrix operator !(FMatrix a) => a.Inverse();
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";

        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {

            StringBuilder sb = new StringBuilder();
            int n = Data.GetLength(1), m = Data.GetLength(0);
            string[][] data = new string[m][];
            int[] maxWidth = new int[m];
            for (int j = 0; j < m; j++)
            {
                data[j] = new string[n];
                for (int i = 0; i < n; i++)
                {
                    double f_val = Math.Round(Data[j, i], HelperFunctions.RoundDigits);
                    data[j][i] = f_val.ToString(formatting, formatProvider);
                }
                maxWidth[j] = data[j].Max((s) => s.Length);
            }

            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                for (int j = 0; j < m; j++)
                {
                    string text = data[j][i];
                    text = text.PadLeft(maxWidth[j]);
                    sb.Append($" {text}");
                }
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }
        #endregion

        #region Collections
        public int Count { get => Rows * Columns; }

        public Span<double> AsSpan()
        {
            fixed (double* ptr = &Data[0, 0])
            {
                return new Span<double>(ptr, Data.Length);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public double[] ToArray()
        {
            double[] data = new double[Data.Length];
            FortranMethods.call_reshape_array_mv(Rows, Columns, Data, data.Length, data);
            return data;
        }

        public bool Contains(double item) => IndexOf(item) >= 0;
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
            Buffer.BlockCopy(Data, 0, array, sizeof(double) * index, Buffer.ByteLength(Data));
        }

        #endregion


    }
}
