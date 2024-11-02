using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Linq.Expressions;
using JA.Fortran.Arrays;

namespace JA.Fortran.Arrays
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
        readonly int _rows;
        readonly int _columns;
        readonly double[,] _data;

        public FMatrix(int rows, int columns)
        {
            _rows = rows;
            _columns = columns;
            this._data = new double[columns, rows];
            Order = ElementOrder.ByColumn;
        }
        internal FMatrix(double[,] data, ElementOrder order = ElementOrder.ByColumn)
        {
            switch (order)
            {
                case ElementOrder.ByRow:
                {
                    _rows=data.GetLength(0);
                    _columns=data.GetLength(1);
                    var temp = new double[_columns,_rows];
                    call_trans_array_m(_rows, _columns, data, temp);
                    this._data=temp;
                    Order=ElementOrder.ByColumn;
                }
                break;
                case ElementOrder.ByColumn:
                {
                    _rows = data.GetLength(1);
                    _columns = data.GetLength(0);
                    this._data=data??throw new ArgumentNullException(nameof(data));
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
            double[,] data = _data;
            call_fill_array_m(rows, columns, values, order, data);
        }
        public FMatrix(int rows, int columns, Func<int, int, double> initializer)
            : this(rows, columns)
        {
            double[,] data = _data;
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
        public static FMatrix RandomMinMax(int n, int m, double minValue = 0, double maxValue = 1)
        {
            double[,] data = new double[m, n];
            call_random_array_m(n, m, minValue, maxValue, data);
            return new FMatrix(data);
        }
        public static FMatrix RandomUniform(int n, int m, ref int seed)
        {
            double[,] data = new double[m, n];
            call_uniform_array_m(n, m, ref seed, data);
            return new FMatrix(data);
        }

        public static FMatrix Zeros(int n) => Zeros(n, n);
        public static FMatrix Zeros(int n, int m)
        {
            double[,] data = new double[m, n];
            call_array_zeros_m(n, m, data);
            return new FMatrix(data);
        }

        public static FMatrix Identity(int n) => Identity(n, n);
        public static FMatrix Identity(int n, int m) => Scalar(n, m, 1.0);
        public static FMatrix Scalar(int n, double value) => Scalar(n, n, value);
        public static FMatrix Scalar(int n, int m, double value)
        {
            double[,] data = new double[m, n];
            call_array_scalar_m(n, m, value, data);
            return new FMatrix(data);
        }
        public static FMatrix Diagonal(double[] value)
        {
            int n = value.Length;
            double[,] data = new double[n, n];
            call_array_diag_m(n, value, data);
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
            call_array_diag_m(size, value, data);
            return new FMatrix(data);
        }
        public static FMatrix FromRows(int rows, int columns, params double[] values)
        {
            return new FMatrix(rows, columns, values, ElementOrder.ByRow);
        }
        public static FMatrix FromColumns(int rows, int columns, params double[] values)
        {
            return new FMatrix(rows, columns, values, ElementOrder.ByColumn);
        }
        public static FMatrix FromColumns(params FVector[] vectors)
        {
            int m = vectors.Length;
            int n = vectors.Length>0 ? vectors[0].Size : 0;
            double[,] data = new double[m, n];
            for (int j = 0; j<m; j++)
            {
                call_array_set_column(n, m, vectors[j].Data, j+1, data);
            }
            return new FMatrix(data);
        }
        public static FMatrix FromRows(params FVector[] vectors)
        {
            int n = vectors.Length;
            int m = vectors.Length>0 ? vectors[0].Size : 0;
            double[,] data = new double[m, n];
            for (int i = 0; i<n; i++)
            {
                call_array_set_row(n, m, vectors[i].Data, i+1, data);
            }
            return new FMatrix(data);
        }
        public static FMatrix FromColumns(FVector[] vectors, FVector b)
        {
            FVector[] all = new FVector[vectors.Length+1];
            vectors.CopyTo(all, 0);
            all[vectors.Length]=b;
            return FromColumns(all);
        }
        public static FMatrix FromRows(FVector[] vectors, FVector b)
        {
            FVector[] all = new FVector[vectors.Length+1];
            vectors.CopyTo(all, 0);
            all[vectors.Length]=b;
            return FromRows(all);
        }
        public FMatrix AppendColumn(FVector a) => FromRows(this.GetRows(), a);
        public FMatrix AppendRow(FVector a) => FromColumns(this.GetColumns(), a);
        public void InsertColumn(FVector a, int column) 
            => call_array_set_column(_rows, _columns, a.Data, column, _data);
        public void InsertRow(FVector a, int row) 
            => call_array_set_row(_rows, _columns, a.Data, row, _data);
        public ElementOrder Order { get; }

        public ref double this[Index row, Index column]
        {
            get
            {
                int i = row.GetOffset(_rows) + (row.IsFromEnd ? 1 : 0);
                int j = column.GetOffset(_columns) + (column.IsFromEnd ? 1 : 0);
                return ref _data[j - 1, i - 1];
            }
        }
        public FMatrix this[Index row, Range columns]
        {
            get
            {
                int i1 = row.GetOffset(_rows) + (row.IsFromEnd ? 1 : 0);
                (int j1, int m1) = columns.GetOffsetAndLength(_rows);
                j1 += columns.Start.IsFromEnd ? 1 : 0;
                int i2 = i1, j2 = j1 + m1;
                return Slice(i1, i2, j1, j2);
            }
        }
        public FMatrix this[Range rows, Index column]
        {
            get
            {
                (int i1, int n1) = rows.GetOffsetAndLength(_rows);
                i1 += rows.Start.IsFromEnd ? 1 : 0;
                int j1 = column.GetOffset(_columns) + (column.IsFromEnd ? 1 : 0);
                int i2 = i1 + n1, j2 = j1;
                return Slice(i1, i2, j1, j2);
            }
        }
        public FMatrix this[Range rows, Range columns]
        {
            get
            {
                (int i1, int n1) = rows.GetOffsetAndLength(_rows);
                i1 += rows.Start.IsFromEnd ? 1 : 0;
                (int j1, int m1) = columns.GetOffsetAndLength(_columns);
                j1 += columns.Start.IsFromEnd ? 1 : 0;
                int i2 = i1 + n1, j2 = j1 + m1;
                return Slice(i1, i2, j1, j2);
            }
        }

        public int Rows => _rows;
        public int Columns => _columns;
        internal double[,] Data => _data;
        public int Size { get => Math.Min(_rows, _columns); }
        public double[,] ToArray2() => _data;

        public static implicit operator double[,](FMatrix a) => a.ToArray2();
        public static explicit operator FMatrix(double[,] a) => new FMatrix(a);

        #region Algebra

        public double Norm() => norm_array_m(_rows, _columns, _data);

        public static FMatrix Round(FMatrix A, int digits)
        {
            double[,] data = new double[A._columns, A.Rows];
            call_round_array_m(A.Rows, A._columns, A.Data, digits, data);
            return new FMatrix(data);
        }

        public FMatrix ReShape(int newRows, int newColumns)
        {
            int n = _rows, m = _columns, k = newRows, l = newColumns;
            double[,] data = new double[l, k];
            call_reshape_array_mm(n, m, _data, k, l, data);
            return new FMatrix(data);
        }
        public FVector ReShape(int newSize)
        {
            int n = _rows, m = _columns, k = newSize;
            double[] data = new double[k];
            call_reshape_array_mv(n, m, _data, k, data);
            return new FVector(data);
        }
        public FMatrix Slice(int startRow, int endRow, int startColumn, int endColumn)
        {
            double[,] data = new double[endColumn - startColumn + 1, endRow - startRow + 1];
            call_slice_array_m(_rows, _columns, _data, startRow, endRow, startColumn, endColumn, data);
            return new FMatrix(data);
        }
        public FVector GetRow(int row)
            => GetRow(row, 1, _columns);
        public FVector GetRow(int row, int startColumn, int endColumn)
        {
            int size = endColumn - startColumn + 1;
            double[,] matrix = new double[size, 1];
            call_slice_array_m(_rows, _columns, _data, row, row, startColumn, endColumn, matrix);
            double[] data = new double[size];
            call_reshape_array_mv(size, 1, matrix, size, data);
            return new FVector(data);
        }
        public FVector GetColumn(int row)
            => GetColumn(row, 1, _columns);
        public FVector GetColumn(int row, int startRow, int endRow)
        {
            int size = endRow - startRow + 1;
            double[,] matrix = new double[1, size];
            call_slice_array_m(_rows, _columns, _data, startRow, endRow, row, row, matrix);
            double[] data = new double[size];
            call_reshape_array_mv(1, size, matrix, size, data);
            return new FVector(data);
        }
        public FVector[] GetColumns()
        {
            int m = _columns;
            int n = _rows;
            FVector[] result = new FVector[_columns];
            double[,] matrix = new double[1, n];
            for (int j = 0; j < m; j++)
            {
                call_slice_array_m(n, m, _data, 1, n, j+1, j+1, matrix);
                var row = new double[m];
                call_reshape_array_mv(n, 1, matrix, m, row);
                result[j]=new FVector(row);
            }
            return result;
        }
        public FVector[] GetRows()
        {
            int m = _columns;
            int n = _rows;
            FVector[] result = new FVector[_rows];
            double[,] matrix = new double[m, 1];
            for (int i = 0; i < n; i++)
            {
                call_slice_array_m(n, m, _data, i+1, i+1, 1, m, matrix);
                var col = new double[n];
                call_reshape_array_mv(1, m, matrix, n, col);
                result[i]=new FVector(col);
            }
            return result;
        }
        public static FMatrix Transpose(FMatrix A)
        {
            int n = A.Rows, m = A._columns;
            double[,] data = new double[n, m];
            call_trans_array_m(n, m, A.Data, data);
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
            if (x.Rows != y.Rows || x._columns != y._columns)
            {
                throw new ArgumentException($"Expecting ({x.Rows},{x._columns}) elements, found ({y.Rows},{y._columns}).", nameof(y));
            }
            int n = x.Rows, m = x._columns;
            double[,] data = new double[m, n];
            call_add_array_m(n, m, x.Data, y.Data, data);
            return new FMatrix(data);
        }
        public static FMatrix Subtract(double x, FMatrix y)
            => Scalar(y.Size, x) - y;
        public static FMatrix Subtract(FMatrix x, double y)
            => x - Scalar(x.Size, y);
        public static FMatrix Subtract(FMatrix x, FMatrix y)
        {
            if (x.Rows != y.Rows || x._columns != y._columns)
            {
                throw new ArgumentException($"Expecting ({x.Rows},{x._columns}) elements, found ({y.Rows},{y._columns}).", nameof(y));
            }
            int n = x.Rows, m = x._columns;
            double[,] data = new double[m, n];
            call_sub_array_m(n, m, x.Data, y.Data, data);
            return new FMatrix(data);
        }
        public static FMatrix Scale(double x, FMatrix A)
        {
            int n = A.Rows, m = A._columns;
            double[,] data = new double[m, n];
            call_scale_array_m(n, m, x, A.Data, data);
            return new FMatrix(data);
        }

        public static FVector Product(FMatrix A, FVector x)
        {
            if (x.Size != A.Rows)
            {
                throw new ArgumentException($"Expecting {x.Size} columns, found {A._columns}.", nameof(A));
            }
            int n = A.Rows, m = A._columns;
            double[] b_data = new double[m];
            call_mul_array_mv(n, m, A.Data, x.Data, b_data);
            return new FVector(b_data);
        }
        public static FVector Product(FVector x, FMatrix A)
        {
            // | A(n,m) | * | x(m) | = | b(n) |
            if (x.Size!=A.Rows)
            {
                throw new ArgumentException($"Expecting {x.Size} rows, found {A.Rows}.", nameof(A));
            }
            int n = A.Rows, m = A._columns;
            double[] data = new double[m];
            call_mul_array_vm(n, m, x.Data, A.Data, data);
            return new FVector(data);
        }

        public static FMatrix Product(FMatrix A, FMatrix X)
        {
            // | A(n,m) | * | x(m,k) | = | b(n,k) |
            int n = A.Rows, m = A._columns, k = X._columns;
            if (X.Rows != m)
            {
                throw new ArgumentException($"Expecting {A._columns} rows, found {X.Rows}.", nameof(X));
            }
            double[,] b_data = new double[k, n];
            call_mul_array_mm(n, m, k, A.Data, X.Data, b_data);
            return new FMatrix(b_data);
        }

        public double Determinant()
        {
            int n = Size;
            call_det_array_m(n, _data, out double det);
            return det;
        }
        public FVector Solve(FVector b)
        {
            // | A(n,n) | * | x(n) | = | b(n) |
            if (b.Size != _rows)
            {
                throw new ArgumentException($"Expecting {_rows} elements, found {b.Size}.", nameof(b));
            }
            int n = _rows, m = _columns;
            double[] data = new double[m];
            call_solve_array_mv(n, _data, b.Data, data);
            return new FVector(data);
        }
        public FMatrix Solve(FMatrix B)
        {
            // | A(n,n) | * | x(n,k) | = | b(n,k) |
            int n = _rows, k = B._columns;
            if (B.Rows != n)
            {
                throw new ArgumentException($"Expecting {n} rows, found {B.Rows}.", nameof(B));
            }
            double[,] data = new double[k, n];
            call_solve_array_mm(n, k, _data, B.Data, data);
            return new FMatrix(data);
        }
        public FMatrix Inverse()
        {
            int n = Size;
            double[,] data = new double[n, n];
            call_inv_array_m(n, _data, data);
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
        public static FVector operator *(FVector v, FMatrix a) => Product(v, a);
        public static FMatrix operator *(FMatrix a, FMatrix b) => Product(a, b);
        public static FMatrix operator /(FMatrix a, double divisor) => Scale(1 / divisor, a);
        public static FMatrix operator ~(FMatrix a) => Transpose(a);
        public static FMatrix operator /(FMatrix X, FMatrix A) => A.Solve(X);
        public static FMatrix operator !(FMatrix a) => a.Inverse();
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";
        public static bool ShowAsTable { get; set; } = true;
        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            if (ShowAsTable)
            {
                return _data.ToTableString(HorizontalAlignment.Right, formatting, formatProvider);
            } else
            {
                return _data.ToListString(formatting);
            }
        }
        #endregion

        #region Collections
        public int Count { get => _rows * _columns; }

        public Span<double> AsSpan()
        {
            fixed (double* ptr = &_data[0, 0])
            {
                return new Span<double>(ptr, _data.Length);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public double[] ToArray()
        {
            double[] data = new double[_data.Length];
            call_reshape_array_mv(_rows, _columns, _data, data.Length, data);
            return data;
        }

        public bool Contains(double item) => IndexOf(item) >= 0;
        public int IndexOf(double item) => Array.IndexOf(_data, item);
        public IEnumerator<double> GetEnumerator()
        {
            for (int j = 0; j < _columns; j++)
            {
                for (int i = 0; i < _rows; i++)
                {
                    yield return _data[j, i];
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
            array = new double[_data.Length];
            Buffer.BlockCopy(_data, 0, array, sizeof(double) * index, Buffer.ByteLength(_data));
        }

        #endregion

        #region Fortran API        
        const string libraryName = FortranMethods.libraryName;

        [DllImport(libraryName, EntryPoint = "call_array_zeros_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_array_zeros_m(int rows, int columns, [Out] double[,] A);
        [DllImport(libraryName, EntryPoint = "call_fill_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_fill_array_m(int rows, int columns, double[] values, ElementOrder order, [Out] double[,] data);

        /// <summary>
        /// Set a column of a matrix from a vector of values.
        /// </summary>
        /// <param name="n">The size of the vector.</param>
        /// <param name="m">The number of columns of the matrix.</param>
        /// <param name="vectors">The vector of values.</param>
        /// <param name="j">The column index.</param>
        /// <param name="data">The resulting matrix data.</param>
        [DllImport(libraryName, EntryPoint = "call_array_set_column", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_array_set_column(int n, int m, double[] vector, int j, [In, Out] double[,] data);
        /// <summary>
        /// Set a row of a matrix from a vector of values.
        /// </summary>
        /// <param name="n">The number rows of the matrix.</param>
        /// <param name="m">The size of the vector.</param>
        /// <param name="vector">The vector of values.</param>
        /// <param name="i">The row index.</param>
        /// <param name="data">The resulting matrix data.</param>
        [DllImport(libraryName, EntryPoint = "call_array_set_row", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_array_set_row(int n, int m, double[] vector, int i, [In, Out] double[,] data);

        [DllImport(libraryName, EntryPoint = "call_random_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_random_array_m(int rows, int columns, [In] double minValue, [In] double maxValue, [Out] double[,] A);
        [DllImport(libraryName, EntryPoint = "call_uniform_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_uniform_array_m(int rows, int columns, ref int seed, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "call_array_diag_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_array_diag_m(int size, [In] double[] x, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "call_array_scalar_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_array_scalar_m(int rows, int columns, [In] double x, [Out] double[,] A);

        [DllImport(libraryName, EntryPoint = "norm_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double norm_array_m(int rows, int columns, [In] double[,] A);

        [DllImport(libraryName, EntryPoint = "call_add_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_add_array_m(int rows, int columns, [In] double[,] x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "call_sub_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_sub_array_m(int rows, int columns, [In] double[,] x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "call_scale_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_scale_array_m(int rows, int columns, double x, [In] double[,] y, [Out] double[,] z);

        [DllImport(libraryName, EntryPoint = "call_det_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_det_array_m(int size, [In] double[,] A, [Out] out double det);

        [DllImport(libraryName, EntryPoint = "call_trans_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_trans_array_m(int rows, int columns, [In] double[,] A, [Out] double[,] At);

        [DllImport(libraryName, EntryPoint = "call_round_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_round_array_m(int rows, int columns, [In] double[,] x, int digits, [Out] double[,] r);
        [DllImport(libraryName, EntryPoint = "call_reshape_array_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_reshape_array_mv(int rows, int columns, [In] double[,] A, int new_size, [Out] double[] B);

        [DllImport(libraryName, EntryPoint = "call_reshape_array_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_reshape_array_mm(int rows, int columns, [In] double[,] A, int new_rows, int new_columns, [Out] double[,] B);

        [DllImport(libraryName, EntryPoint = "call_slice_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_slice_array_m(int rows, int columns, [In] double[,] A, int start_row, int end_row, int start_column, int end_column, [Out] double[,] B);

        /// <summary>
        /// <code><![CDATA[
        /// pure subroutine call_slice_rows_array_m(n,m,A,i,b) bind(c)
        /// integer, intent(in), value :: n, m, i
        /// real(real64), intent(in) :: A(n,m)
        /// real(real64), intent(out) :: b(m)]]></code>
        /// </summary>
        /// <param name="rows">The row count of A.</param>
        /// <param name="columns">The column count of A.</param>
        /// <param name="A">The input matrix</param>
        /// <param name="row">The row to slice.</param>
        /// <param name="b">The resulting vector.</param>
        /// <remarks><c>b = A(i, :)</c></remarks>
        [DllImport(libraryName, EntryPoint = "call_slice_rows_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_slice_rows_array_m(
            int rows, 
            int columns, 
            [In] double[,] A, 
            int row, 
            [Out] double[] b);

        /// <summary>
        /// <code><![CDATA[
        /// pure subroutine call_slice_cols_array_m(n,m,A,j,b) bind(c)
        /// integer, intent(in), value :: n, m, j
        /// real(real64), intent(in) :: A(n,m)
        /// real(real64), intent(out) :: b(n)]]></code>
        /// </summary>
        /// <param name="rows">The row count of A.</param>
        /// <param name="columns">The column count of A.</param>
        /// <param name="A">The input matrix</param>
        /// <param name="col">The column to slice.</param>
        /// <param name="b">The resulting vector.</param>
        /// <remarks><c>b = A(:, j)</c></remarks>
        [DllImport(libraryName, EntryPoint = "call_slice_cols_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_slice_cols_array_m(
            int rows, 
            int columns, 
            [In] double[,] A, 
            int col, 
            [Out] double[] b);

        /// <summary>
        /// <code><![CDATA[
        /// pure subroutine call_slice_row_array_m(n,m,A,i,j1,j2,b) bind(c)
        /// integer, intent(in), value :: n, m, i, j1, j2
        /// real(real64), intent(in) :: A(n,m)
        /// real(real64), intent(out) :: b(j2-j1+1)]]></code>
        /// </summary>
        /// <param name="rows">The row count of A.</param>
        /// <param name="columns">The column count of A.</param>
        /// <param name="A">The input matrix</param>
        /// <param name="row">The row to slice.</param>
        /// <param name="start_col">The start column of the row</param>
        /// <param name="end_col">The end column of the row</param>
        /// <param name="b">The resulting vector.</param>
        /// <remarks><c>b = A(i, j1:j2)</c></remarks>
        [DllImport(libraryName, EntryPoint = "call_slice_row_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_slice_row_array_m(
            int rows, 
            int columns, 
            [In] double[,] A, 
            int row, 
            int start_col,
            int end_col,
            [Out] double[] b);

        /// <summary>
        /// <code><![CDATA[
        /// pure subroutine call_slice_col_array_m(n,m,A,i1,i2,j,b) bind(c)
        /// integer, intent(in), value :: n, m, i1,i2,j
        /// real(real64), intent(in) :: A(n,m)
        /// real(real64), intent(out) :: b(i2-i1+1)]]></code>
        /// </summary>
        /// <param name="rows">The row count of A.</param>
        /// <param name="columns">The column count of A.</param>
        /// <param name="A">The input matrix</param>
        /// <param name="col">The column to slice.</param>
        /// <param name="start_col">The starting column of the row</param>
        /// <param name="end_col">The end column of the row</param>
        /// <param name="b">The resulting vector.</param>
        /// <remarks><c>b = A(i1:i2, j)</c></remarks>
        [DllImport(libraryName, EntryPoint = "call_slice_col_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_slice_col_array_m(
            int rows, 
            int columns, 
            [In] double[,] A, 
            int col, 
            int start_col,
            int end_col,
            [Out] double[] b);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>b=A*x</code>.
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="x">The known vector</param>
        /// <param name="b">The result vector</param>
        [DllImport(libraryName, EntryPoint = "call_mul_array_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_mul_array_mv(int rows, int columns, [In] double[,] A, [In] double[] x, [Out] double[] b);

        /// <summary>
        /// Fortran DLL call to matrix multiply <code>b=x*A</code>.
        /// <code><![CDATA[subroutine call_mul_array_vm(n,m,x,A,b) bind(c)
        ///integer, intent(in), value :: n, m
        ///real(real64), intent(in) :: A(n,m), x(m)
        ///real(real64), intent(out) :: b(n)]]>
        /// </code>
        /// </summary>
        /// <param name="x">The known vector</param>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="b">The result vector</param>
        [DllImport(libraryName, EntryPoint = "call_mul_array_vm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_mul_array_vm(int rows, int columns, [In] double[] x, [In] double[,] A, [Out] double[] b);
        /// <summary>
        /// Fortran DLL call to matrix multiply <c>C=A*B</c>
        /// <code><![CDATA[subroutine call_mul_array_mm(n,m,k,A,x,b) bind(c)
        ///integer, intent(in), value :: n, m, k
        ///real(real64), intent(in) :: A(n,m), x(m,k)
        ///real(real64), intent(out) :: b(n,k)]]>
        ///</code>
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="B">The known matrix</param>
        /// <param name="C">The result matrix</param>
        [DllImport(libraryName, EntryPoint = "call_mul_array_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_mul_array_mm(int rows, int columns, int pages, [In] double[,] A, [In] double[,] x, [Out] double[,] b);

        /// <summary>
        /// Fortran DLL call to solve the linear system of equations <c>A*x=b</c> for <paramref name="x"/>.
        /// <code><![CDATA[subroutine call_solve_array_mv(n,A,b,x) bind(c)
        ///use mod_array_inv
        ///integer, intent(in), value :: n
        ///real(real64), intent(in) :: A(n,n), b(n)
        ///real(real64), intent(out) :: x(n) ]]>
        /// </code>
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="b">The known vector</param>
        /// <param name="x">The unknown vector</param>
        [DllImport(libraryName, EntryPoint = "call_solve_array_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_solve_array_mv(int rows, [In] double[,] A, [In] double[] b, [Out] double[] x);
        /// <summary>
        /// Fortran DLL call to solve the linear system of equations <c>A*X=B</c> for <paramref name="X"/>.
        /// <code><![CDATA[subroutine call_solve_array_mm(n,k,A,b,x) bind(c)
        ///integer, intent(in), value :: n,k
        ///real(real64), intent(in) :: A(n,n), b(n,k)
        ///real(real64), intent(out) :: x(n,k)]]>
        /// </code>
        /// </summary>
        /// <param name="A">The coefficient matrix.</param>
        /// <param name="B">The known matrix. </param>
        /// <param name="X">The unknown matrix.</param>
        [DllImport(libraryName, EntryPoint = "call_solve_array_mm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_solve_array_mm(int rows, int pages, [In] double[,] A, [In] double[,] B, [Out] double[,] X);

        [DllImport(libraryName, EntryPoint = "call_inv_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_inv_array_m(int size, [In] double[,] A, [Out] double[,] A_inv);
        /// <summary>
        /// Matrix inner product.
        /// <code><![CDATA[
        /// pure subroutine call_inner_array_m(n,m,x,y,z) bind(c)
        /// integer, intent(in), value :: n, m, k
        /// real(real64), intent(in) :: x(n,m), y(n,k)
        /// real(real64), intent(out) :: z(m,k)]]></code>
        /// </summary>
        /// <param name="rows">Rows of x</param>
        /// <param name="columns">Columns of x and y</param>
        /// <param name="pages">Columns of y</param>
        /// <param name="x">Input matrix x(n,m).</param>
        /// <param name="y">Input matrix y(n,k).</param>
        /// <param name="z">Result matrix z(m,k).</param>
        /// <remarks><c>z = trans(x)*y</c></remarks>
        [DllImport(libraryName, EntryPoint = "call_inner_array_m", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_inner_array_m(int rows, int columns, int pages, [In] double[,] x, [In] double[,] y, [Out] out double[,] z);

        #endregion
    }
}
