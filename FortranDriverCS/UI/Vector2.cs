using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using JA.Fortran;

namespace JA.UI
{
    public readonly struct Vector2 :
        IEquatable<Vector2>,
        IReadOnlyList<double>,
        ICollection<double>,
        System.Collections.ICollection,
        IFormattable
    {
        static readonly Random rng = new Random();
        readonly (double x, double y) data;

        #region Factory
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public Vector2(double x, double y)
        {
            data = (x, y);
        }

        public static implicit operator Vector2(System.Numerics.Vector2 vector)
            => new Vector2(vector.X, vector.Y);
        public static explicit operator System.Numerics.Vector2(Vector2 vector)
            => new System.Numerics.Vector2((float)vector.X, (float)vector.Y);

        public static Vector2 Zero { get; } = new Vector2(0, 0);
        public static Vector2 UnitX { get; } = new Vector2(1, 0);
        public static Vector2 UnitY { get; } = new Vector2(0, 1);
        public static Vector2 One { get; } = new Vector2(1, 1);

        public static Vector2 Cartesian(double x, double y) => new Vector2(x, y);
        public static Vector2 Polar(double r, double θ)
            => new Vector2(
            r * Math.Cos(θ),
            r * Math.Sin(θ));
        public static Vector2 Elliptical(double a, double b, double θ)
            => new Vector2(
            a * Math.Cos(θ),
            b * Math.Sin(θ));

        public static Vector2 Random(double minValue = 0, double maxValue = 1)
            => new Vector2(
                minValue + (maxValue - minValue) * rng.NextDouble(),
                minValue + (maxValue - minValue) * rng.NextDouble());

        #endregion

        #region Properties
        /// <summary>The X component of the vector.</summary>
        public double X => data.x;
        /// <summary>The Y component of the vector.</summary>
        public double Y => data.y;
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public double SumSquares() => data.x * data.x + data.y * data.y;
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public double Magnitude() => Math.Sqrt(SumSquares());
        public (double r, double θ) ToPolar() => (Magnitude(), Math.Atan2(Y, X));

        public bool IsZero { get => data.x == 0 && data.y == 0; }

        public void Deconstruct(out double x, out double y)
        {
            (x, y) = data;
        }
        #endregion

        #region Algebra
        public static Vector2 Negate(Vector2 a)
            => new Vector2(
                -a.data.x,
                -a.data.y);
        public static Vector2 Scale(double factor, Vector2 a)
            => new Vector2(
                factor * a.data.x,
                factor * a.data.y);
        public static Vector2 Add(Vector2 a, Vector2 b)
            => new Vector2(
                a.data.x + b.data.x,
                a.data.y + b.data.y);
        public static Vector2 Subtract(Vector2 a, Vector2 b)
            => new Vector2(
                a.data.x - b.data.x,
                a.data.y - b.data.y);

        public static Vector2 operator +(Vector2 a, Vector2 b) => Add(a, b);
        public static Vector2 operator -(Vector2 a) => Negate(a);
        public static Vector2 operator -(Vector2 a, Vector2 b) => Subtract(a, b);
        public static Vector2 operator *(double f, Vector2 a) => Scale(f, a);
        public static Vector2 operator *(Vector2 a, double f) => Scale(f, a);
        public static Vector2 operator /(Vector2 a, double d) => Scale(1 / d, a);

        #endregion

        #region Vector Algebra        
        /// <summary>
        /// Scale the vector such that the magnitude is one (if possible).
        /// </summary>
        /// <param name="vector">The vector.</param>
        /// <returns>A normalized vector.</returns>
        public static Vector2 Normalize(Vector2 vector)
        {
            double m = vector.Magnitude();
            return vector / m;
        }

        /// <summary>
        /// Calculate the Eucledian distance between two points.
        /// </summary>
        /// <param name="fromPoint">From point.</param>
        /// <param name="toPoint">To point.</param>
        public static double Distance(Vector2 fromPoint, Vector2 toPoint)
            => (toPoint - fromPoint).Magnitude();

        /// <summary>
        /// Calculate the Eucledian distance squared between two points.
        /// </summary>
        /// <remarks>Faster than regular distance because it does not involve
        /// and square-root function.</remarks>
        /// <param name="fromPoint">From point.</param>
        /// <param name="toPoint">To point.</param>
        public static double DistanceSquared(Vector2 fromPoint, Vector2 toPoint)
            => (toPoint - fromPoint).SumSquares();

        /// <summary>
        /// Get the direction vector between two points.
        /// </summary>
        /// <param name="fromPoint">From point.</param>
        /// <param name="toPoint">To point.</param>
        public static Vector2 Direction(Vector2 fromPoint, Vector2 toPoint)
            => Normalize(toPoint - fromPoint);

        /// <summary>
        /// Dot product of two vectors.
        /// </summary>
        public static double Dot(Vector2 a, Vector2 b)
            => a.data.x * b.data.x + a.data.y * b.data.y;

#if VECTOR_ALGEBRA
        /// <summary>
        /// Outer product of two vectors.
        /// </summary>
        public static Matrix2 Outer(Vector2 a, Vector2 b)
            => new Matrix2(
                a.data.x*b.data.x, a.data.x*b.data.y,
                a.data.y*b.data.x, a.data.y*b.data.y);

        public static Vector2 Transform(Vector2 vector, Rotor2 rotor)
        {
            double cos = 1-2*rotor.Z*rotor.Z;
            double sin = 2*rotor.W*rotor.Z;

            return new Vector2(
                cos*vector.X-sin*vector.Y,
                sin*vector.X+cos*vector.Y);
        }

        public static Vector2 Transform(Vector2 vector, Matrix2 matrix)
        {
            return new Vector2(
                matrix.A11*vector.X + matrix.A12*vector.Y,
                matrix.A21*vector.X + matrix.A22*vector.Y);
        }
#endif
        /// <summary>
        /// Cross product of two vectors.
        /// </summary>
        /// <returns>An out of plane value</returns>
        public static double Cross(Vector2 a, Vector2 b)
            => a.data.x * b.data.y - a.data.y * b.data.x;
        /// <summary>
        /// Cross product of a vector with an out-of-plane value.
        /// </summary>
        /// <returns>A vector</returns>
        public static Vector2 Cross(Vector2 a, double b)
            => new Vector2(a.data.y * b, -a.data.x * b);
        /// <summary>
        /// Cross product of a out-of-plane value with a vector.
        /// </summary>
        /// <returns>A vector</returns>
        public static Vector2 Cross(double a, Vector2 b)
            => Cross(b, -a);

        /// <summary>Performs a linear interpolation between two vectors based on the given weighting.</summary>
        /// <param name="value1">The first vector.</param>
        /// <param name="value2">The second vector.</param>
        /// <param name="amount">A value between 0 and 1 that indicates the weight of <paramref name="value2" />.</param>
        /// <returns>The interpolated vector.</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Vector2 Lerp(Vector2 value1, Vector2 value2, double amount)
        {
            Vector2 left = value1 * (1 - amount);
            Vector2 right = value2 * amount;
            return left + right;
        }



        #endregion

        #region Equality
        public static bool operator ==(Vector2 vector1, Vector2 vector2) => vector1.Equals(vector2);

        public static bool operator !=(Vector2 vector1, Vector2 vector2) => !(vector1 == vector2);
        public override bool Equals(object obj)
        {
            return obj is Vector2 vector && Equals(vector);
        }
        public bool Equals(Vector2 other) => data.Equals(other.data);

        public override int GetHashCode()
        {
            int hashCode = 1768953197;
            hashCode = hashCode * -1521134295 + data.GetHashCode();
            return hashCode;
        }
        #endregion

        #region Collections
        /// <summary>
        /// Gets a value indicating whether this array is of fixed size.
        /// </summary>
        public bool IsReadOnly => true;
        /// <summary>
        /// Get the number of elements in the vector.
        /// </summary>
        public int Count => 2;

        public double this[int index]
        {
            get
            {
                switch (index)
                {
                    case 0: return data.x;
                    case 1: return data.y;
                    default:
                        throw new ArgumentOutOfRangeException();
                }
            }
        }
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void CopyTo(double[] array, int index)
            => ToArray().CopyTo(array, index);
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void CopyTo(Array array, int index)
            => ToArray().CopyTo(array, index);
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public double[] ToArray() => AsSpan().ToArray();
        public unsafe ReadOnlySpan<double> AsSpan()
        {
            fixed (double* ptr = &data.x)
            {
                return new ReadOnlySpan<double>(ptr, 2);
            }
        }
        public System.Collections.IEnumerator GetEnumerator() => ToArray().GetEnumerator();
        public IEnumerable<double> AsEnumerable() => this as IEnumerable<double>;
        IEnumerator<double> IEnumerable<double>.GetEnumerator()
        {
            yield return data.x;
            yield return data.y;
        }
        void ICollection<double>.Add(double item) => throw new NotSupportedException();
        void ICollection<double>.Clear() => throw new NotSupportedException();
        bool ICollection<double>.Remove(double item) => throw new NotSupportedException();
        bool ICollection<double>.Contains(double item) => throw new NotSupportedException();
        object System.Collections.ICollection.SyncRoot { get => null; }
        bool System.Collections.ICollection.IsSynchronized { get => false; }
        #endregion

        #region Formatting
        public string ToString(string formatting, IFormatProvider provider)
        {
            string x = data.x.ToString(formatting, provider);
            string y = data.y.ToString(formatting, provider);
            return $"({x},{y})";
        }
        public string ToString(string formatting)
            => ToString(formatting, null);
        public override string ToString()
            => ToString("g");

        #endregion

    }
}
