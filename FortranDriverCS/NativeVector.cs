using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.CompilerServices;

namespace FortranDriver
{

    public unsafe class NativeVector :
        System.Collections.ICollection,
        ICollection<double>,
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
        public static NativeVector FromValues(params double[] values)
        {
            return new NativeVector(values);
        }

        public static NativeVector Elemental(int size, int index, double value = 1.0)
        {
            double[] data = new double[size];
            FortranMethods.array_elem_v(size, index, value, data);
            return new NativeVector(data);
        }
        private static int seed = Environment.TickCount;
        public static NativeVector RandomMinMax(int size, double minValue = 0, double maxValue = 1)
        {
            double[] data = new double[size];
            FortranMethods.array_random_v(size, minValue, maxValue, data);
            return new NativeVector(data);
        }
        public static NativeVector RandomUniform(int size)
        {
            double[] data = new double[size];
            FortranMethods.array_uniform_v(size, ref seed, data);
            return new NativeVector(data);
        }

        public ref double this[Index index]
        {
            get
            {
                int i = index.GetOffset(Size);
                return ref Data[i-1];
            }
        }

        public NativeVector this[Range range]
        {
            get
            {
                var (offset, length)= range.GetOffsetAndLength(Size);
                int i1 = offset, i2 = offset + length;
                return Slice(i1, i2);
            }
        }

        public int Size { get; }
        internal double[] Data { get; }


        public static implicit operator double[](NativeVector a) => a.ToArray();
        public static explicit operator NativeVector(double[] a) => new NativeVector(a);

        #region Algebra
        public static NativeVector Round(NativeVector A, int digits)
        {
            double[] data = new double[A.Size];
            FortranMethods.array_round_v(A.Size, A.Data, digits, data);
            return new NativeVector(data);
        }

        public NativeMatrix ReShape(int newRows, int newColumns)
        {
            int n = Size, k = newRows, l = newColumns;
            double[,] data = new double[l, k];
            FortranMethods.array_reshape_vm(n, Data, k, l, data);
            return new NativeMatrix(data);
        }
        public NativeVector Slice(int startRow, int endRow)
        {
            double[] data = new double[endRow-startRow+1];
            FortranMethods.array_slice_v(Size, Data, startRow, endRow, data);
            return new NativeVector(data);
        }
        public static double Dot(NativeVector x, NativeVector y)
        {
            int size = Math.Min(x.Size, y.Size);
            FortranMethods.array_dot_v(size, x.Data, y.Data, out var result);
            return result;
        }
        public static NativeVector Negate(NativeVector x)
            => Scale(-1, x);
        public static NativeVector Add(NativeVector x, NativeVector y)
        {
            if (x.Size != y.Size)
            {
                throw new ArgumentException($"Expecting {x.Size} elements, found {y.Size}.", nameof(y));
            }
            int n = x.Size;
            double[] data = new double[n];
            FortranMethods.array_add_v(n, x.Data, y.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Subtract(NativeVector x, NativeVector y)
        {
            if (x.Size != y.Size)
            {
                throw new ArgumentException($"Expecting {x.Size} elements, found {y.Size}.", nameof(y));
            }
            int n = x.Size;
            double[] data = new double[n];
            FortranMethods.array_subtract_v(n, x.Data, y.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Scale(double x, NativeVector A)
        {
            int n = A.Size;
            double[] data = new double[n];
            FortranMethods.array_scale_v(n, x, A.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Product(NativeVector x, NativeMatrix A)
        {
            // | A(n,m) | * | x(m) | = | b(n) |
            if (x.Size != A.Rows)
            {
                throw new ArgumentException($"Expecting {x.Size} rows, found {A.Rows}.", nameof(A));
            }
            int n = A.Rows, m = A.Columns;
            double[] data = new double[m];
            FortranMethods.array_product_vm(n, m, x.Data, A.Data, data);
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
        public static NativeVector operator /(NativeVector x, NativeMatrix A) => A.Solve(x);
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";

        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            //double[] data = new double[Size];
            //FortranMethods.array_round_v(Size, Data, 11, data);

            StringBuilder sb = new StringBuilder();
            int n = Data.Length;
            string[][] data = [new string[n]];
            for (int i = 0; i < n; i++)
            {
                data[0][i] = Data[i].ToString(formatting, formatProvider);
            }
            int maxWidth = data[0].Max((s) => s.Length);
            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                var text = data[0][i].PadLeft(maxWidth);
                sb.Append($" {text}");
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }
        #endregion

        #region Collections
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public double[] ToArray() => Data;
        public int Count => Data.Length;
        public Span<double> AsSpan()
        {
            fixed (double* ptr = &Data[0])
            {
                return new Span<double>(ptr, Data.Length);
            }
        }
        public bool Contains(double item) => IndexOf(item)>=0;
        public int IndexOf(double item) => Array.IndexOf(Data, item);
        public IEnumerator<double> GetEnumerator()
        {
            for (int i = 0; i < Data.Length; i++)
            {
                yield return Data[i];
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
            => CopyTo(array as double[], index);
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void CopyTo(double[] array, int index)
            => Array.Copy(Data, 0, array, index, Data.Length);
        #endregion




    }
}
