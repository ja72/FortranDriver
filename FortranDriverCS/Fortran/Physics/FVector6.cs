using System.Diagnostics.Contracts;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;
using JA.Fortran.Arrays;
using Windows.ApplicationModel.Activation;

namespace JA.Fortran.Physics
{
    public unsafe struct FVector6 : 
        IFormattable,
        IEquatable<FVector6>
    {
        private const int _size = 6;
        public static int Size { get; } = _size;

        fixed double _data[_size];

        #region Factory
        public FVector6(FVector3 a, FVector3 b)
        {
            this=vec6_block(a, b);
        }
        public FVector6(double[] values, int index = 0)
        {
            Contract.Requires(null!=values);
            Contract.Requires(index+_size<=values.Length);
            fixed (double* ptr = _data)
            {
                for (int i = 0; i<_size; i++)
                {
                    _data[i]=values[i];
                }
            }
        }
        public static FVector6 Twist(in FVector3 vector, in FVector3 position, double pitch)
            => vec6_twist_pos_pitch(vector, position, pitch);
        public static FVector6 Twist(in FVector3 vector, in FVector3 position)
            => vec6_twist_pos(vector, position);
        public static FVector6 Twist(in FVector3 vector)
            => vec6_twist_pure(vector);
        public static FVector6 Wrench(in FVector3 vector, in FVector3 position, double pitch)
            => vec6_wrench_pos_pitch(vector, position, pitch);
        public static FVector6 Wrench(in FVector3 vector, in FVector3 position)
            => vec6_wrench_pos(vector, position);
        public static FVector6 Wrench(in FVector3 vector)
            => vec6_wrench_pure(vector);

        public static implicit operator FVector6(double[] values) => new FVector6(values, 0);
        public static FVector6 Zero { get; } = vec6_zeros();
        #endregion

        #region Properties
        public double E1 => _data[0];
        public double E2 => _data[1];
        public double E3 => _data[2];
        public double E4 => _data[3];
        public double E5 => _data[4];
        public double E6 => _data[5];

        public Span<double> AsSpan()
        {
            fixed (double* ptr = _data)
            {                    
                return new Span<double>(ptr, _size);
            }
        }

        public ref double this[int index] => ref _data[index];
        public readonly double Norm() => norm_vec6(this);

        public readonly double[] ToArray()
        {
            double[] data = new double[6];
            call_vec6_to_array(this, data);
            return data;
        }
        public readonly FVector ToVector() => new FVector(ToArray());

        #endregion

        #region Formatting
        public static string DefaultFormatting { get; set; } = "g6";
        public static bool ShowAsTable { get; set; } = true;
        public override string ToString() => ToString(DefaultFormatting);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            if (ShowAsTable)
            {
                return ToArray().ToVectorTableString(HorizontalAlignment.Right, formatting, formatProvider);
            }
            else
            {
                return ToArray().ToListString(formatting);
            }
        }
        #endregion

        #region IEquatable Members
        /// <summary>
        /// Equality overrides from <see cref="System.Object"/>
        /// </summary>
        /// <param name="obj">The object to compare this with</param>
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FVector6)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FVector6 vector&&Equals(vector);
        }

        public static bool operator ==(FVector6 target, FVector6 other) => target.Equals(other);
        public static bool operator !=(FVector6 target, FVector6 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FVector6"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FVector6"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FVector6 other)
        {
            for (int i = 0; i<_size; i++)
            {
                if (_data[i]!=other._data[i]) return false;
            }
            return true;
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FVector6"/>
        /// </summary>
        /// <returns>The int hash value</returns>
        public override int GetHashCode()
        {
            unchecked
            {
                int hc = -1817952719;
                for (int i = 0; i<_size; i++)
                {
                    hc=( -1521134295 )*hc+_data[i].GetHashCode();
                }
                return hc;
            }
        }

        #endregion

        #region Algebra
        public static FVector6 Add(in FVector6 a, in FVector6 b) => add_vec6_vec6(a, b);
        public static FVector6 Subtract(in FVector6 a, in FVector6 b) => sub_vec6_vec6(a, b);
        public static FVector6 Negate(in FVector6 a) => neg_vec6(a);
        public static FVector6 Scale(double factor, in FVector6 a) => mul_scalar_vec6(factor, a);
        public static FVector6 Scale(in FVector6 a, double factor) => mul_vec6_scalar(a, factor);
        public static FVector6 Divide(in FVector6 a, double d) => div_vec6_scalar(a, d);
        public static double Dot(in FVector6 A, in FVector6 B) => dot_vec6_vec6(A, B);
        public static FMatrix3 Outer(in FVector6 a, in FVector6 b) => outer_vec6_vec6(a, b);
        public static FVector6 CrossTwist(in FVector6 a, in FVector6 b) => cross_twist_twist(a, b);
        public static FVector6 CrossWrench(in FVector6 a, in FVector6 b) => cross_twist_wrench(a, b);
        public readonly FMatrix6 CrossTwistOp() => cross_twist_twist_op(this);
        public readonly FMatrix6 CrossWrenchOp() => cross_twist_wrench_op(this);
        #endregion

        #region Operators
        public static FVector6 operator +(in FVector6 a, in FVector6 b) => Add(a, b);
        public static FVector6 operator -(in FVector6 a) => Negate(a);
        public static FVector6 operator -(in FVector6 a, in FVector6 b) => Subtract(a, b);
        public static FVector6 operator *(double f, in FVector6 a) => Scale(f, a);
        public static FVector6 operator *(in FVector6 a, double f) => Scale(f, a);
        public static FVector6 operator /(in FVector6 a, double d) => Divide(a, d);
        public static double operator |(in FVector6 a, in FVector6 b) => Dot(a, b);
        #endregion

        #region Fortran API

        const string libraryName = FortranMethods.libraryName;

        [DllImport(libraryName, EntryPoint = "vec6_block", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 vec6_block(in FVector3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "vec6_zeros", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 vec6_zeros();
        [DllImport(libraryName, EntryPoint = "norm_vec6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double norm_vec6(in FVector6 a);
        [DllImport(libraryName, EntryPoint = "add_vec6_vec6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 add_vec6_vec6(in FVector6 a, in FVector6 b);
        [DllImport(libraryName, EntryPoint = "sub_vec6_vec6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 sub_vec6_vec6(in FVector6 a, in FVector6 b);
        [DllImport(libraryName, EntryPoint = "neg_vec6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 neg_vec6(in FVector6 a);
        [DllImport(libraryName, EntryPoint = "mul_scalar_vec6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 mul_scalar_vec6(double s, in FVector6 b);
        [DllImport(libraryName, EntryPoint = "mul_vec6_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 mul_vec6_scalar(in FVector6 a, double s);
        [DllImport(libraryName, EntryPoint = "div_vec6_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 div_vec6_scalar(in FVector6 a, double s);
        [DllImport(libraryName, EntryPoint = "dot_vec6_vec6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double dot_vec6_vec6(in FVector6 a, in FVector6 b  );
        [DllImport(libraryName, EntryPoint = "outer_vec6_vec6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 outer_vec6_vec6(in FVector6 a, in FVector6 b);
        [DllImport(libraryName, EntryPoint = "call_vec6_to_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_vec6_to_array(in FVector6 a, [Out] double[] b);
        [DllImport(libraryName, EntryPoint = "cross_twist_twist", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 cross_twist_twist(in FVector6 a, in FVector6 b);
        [DllImport(libraryName, EntryPoint = "cross_twist_wrench", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 cross_twist_wrench(in FVector6 a, in FVector6 b);
        [DllImport(libraryName, EntryPoint = "cross_twist_twist", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 cross_twist_twist_op (in FVector6 a);
        [DllImport(libraryName, EntryPoint = "cross_twist_wrench", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 cross_twist_wrench_op(in FVector6 a);
        [DllImport(libraryName, EntryPoint = "vec6_first", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 vec6_first(in FVector6 a);
        [DllImport(libraryName, EntryPoint = "vec6_second", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 vec6_second(in FVector6 a);
        [DllImport(libraryName, EntryPoint = "vec6_twist_pos_pitch", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 vec6_twist_pos_pitch(in FVector3 a, in FVector3 r, double h);
        [DllImport(libraryName, EntryPoint = "vec6_twist_pos", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 vec6_twist_pos(in FVector3 a, in FVector3 r);
        [DllImport(libraryName, EntryPoint = "vec6_twist_pure", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 vec6_twist_pure(in FVector3 a);
        [DllImport(libraryName, EntryPoint = "vec6_wrench_pos_pitch", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 vec6_wrench_pos_pitch(in FVector3 a, in FVector3 r, double h);
        [DllImport(libraryName, EntryPoint = "vec6_wrench_pos", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 vec6_wrench_pos(in FVector3 a, in FVector3 r);
        [DllImport(libraryName, EntryPoint = "vec6_wrench_pure", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 vec6_wrench_pure(in FVector3 a);
        
        #endregion
    }
}
