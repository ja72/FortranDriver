using System;
using System.Diagnostics.Contracts;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;

using static System.Runtime.InteropServices.JavaScript.JSType;

namespace JA.Fortran
{
    public unsafe struct FQuat4 : 
        IFormattable,
        IEquatable<FQuat4>
    {
        const int _size = 4;
        public static int Size { get; } = _size;

        fixed double _data[_size];

        #region Factory
        public FQuat4(double scalar) : this(scalar, 0, 0, 0) { }
        public FQuat4(FVector3 vector) : this(0, vector.X, vector.Y, vector.X) { }
        public FQuat4(double s, double vx, double vy, double vz)
        {
            this=FromValues(s, vx, vy, vz);
        }
        public FQuat4(double[] values, int index = 0)
        {
            Contract.Requires(null!=values);
            Contract.Requires(index+_size<=values.Length);
            this=FromArray(values);
        }
        public static FQuat4 FromScalarVector(double scalar, FVector3 vector)
            => quat4_scalar_vec3(scalar, vector);
        public static FQuat4 FromValues(double w, double x, double y, double z)
            => quat4_values(w, x, y, z);
        public static FQuat4 FromArray(double[] values)
            => quat4_array(values);

        public static implicit operator FQuat4(double[] values) => new FQuat4(values, 0);
        public static implicit operator FQuat4(FVector3 vector) => FromScalarVector(0, vector);
        public static FQuat4 Zero { get; } = quat4_zeros();
        public static FQuat4 EX { get; } = quat4_values(0,1,0,0);
        public static FQuat4 EY { get; } = quat4_values(0,0,1,0);
        public static FQuat4 EZ { get; } = quat4_values(0,0,0,1);
        public static FQuat4 Identity { get; } = quat4_identity();
        public static FQuat4 Uniform(ref int seed) => quat4_uniform(ref seed);
        public static FQuat4 FromRotation(FVector3 axis, double angle)
            => quat4_axis_angle(axis, angle);
        public static FQuat4 FromRotation(FMatrix3 rotation) 
            => quat4_from_matrix(rotation);
        public static FQuat4 FromRotationX(double angle)
            => quat4_axis_angle(FVector3.EX, angle);
        public static FQuat4 FromRotationY(double angle)
            => quat4_axis_angle(FVector3.EY, angle);
        public static FQuat4 FromRotationZ(double angle)
            => quat4_axis_angle(FVector3.EZ, angle);
        #endregion

        #region Properties
        public ref double S =>  ref _data[0];
        public ref double VX => ref _data[1];
        public ref double VY => ref _data[2];
        public ref double VZ => ref _data[3];

        public ref double this[Index index]
        {
            get
            {
                int i = index.GetOffset(_size);
                return ref _data[i];
            }
        }

        public double[] ToArray() => AsSpan().ToArray();

        public Span<double> AsSpan()
        {
            fixed (double* ptr = _data)
            {                    
                return new Span<double>(ptr, _size);
            }
        }
        public readonly FMatrix3 ToRotation(bool inverse = false)
            => quat4_to_matrix(this, inverse);
        public readonly void ToAxisAngle(out FVector3 axis, out double angle) 
            => quat4_to_axis_angle(in this, out axis, out angle);
        public ref double this[int index] => ref _data[index];
        public readonly double Scalar() => quat4_scalar(this);
        public readonly FVector3 Vector() => quat4_vector(this);
        public readonly double Norm() => norm_quat4(this);
        public readonly FQuat4 Conjugate() => quat4_conjugate(this);
        public readonly FQuat4 Inverse() => quat4_inverse(this);
        public readonly FVector3 RotateVector(FVector3 vector, bool inverse = false) => quat4_rotate_vec3(this, vector, inverse);
        public readonly FMatrix3 RotateDiagonal(double[] diag,bool inverse = false) => quat4_rotate_diag(this, diag, inverse);
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
                var block = new double[2][];
                block[0]= [this.S];
                block[1]= [this.VX, this.VY, this.VZ];
                return block.ToBlockTableString(HorizontalAlignment.Right, formatting, formatProvider);
            }
            else
            {
                string sw = Math.Round(  S, HelperFunctions.RoundDigits).ToString<double>(formatting, formatProvider);
                string vx = Math.Round( VX, HelperFunctions.RoundDigits).ToString<double>(formatting, formatProvider);
                string vy = Math.Round( VY, HelperFunctions.RoundDigits).ToString<double>(formatting, formatProvider);
                string vz = Math.Round( VZ, HelperFunctions.RoundDigits).ToString<double>(formatting, formatProvider);

                return $"[{sw}|{vx},{vy},{vz}]";
            }
        }
        public string ToFixedColumnString(string formatting = null, int width = HelperFunctions.DefaultColumnWidth)
        {
            formatting??=DefaultFormatting;
            StringBuilder sb = new StringBuilder();
            var data = ToArray();
            for (int i = 0; i < data.Length; i++)
            {
                sb.Append('|');
                var f_val = Math.Round(data[i], HelperFunctions.RoundDigits);
                string text = f_val.ToString(formatting, CultureInfo.CurrentCulture.NumberFormat);
                text = text.PadLeft(width);
                if (text.Length > width)
                {
#pragma warning disable IDE0057 // Use range operator
                    text = new string('*', width);
#pragma warning restore IDE0057 // Use range operator
                }
                sb.Append($" {text}");
                sb.AppendLine(" |");
                if (i == 0)
                {
                    text = new string('-', width);
                    sb.AppendLine($"| {text} |");
                }
            }
            sb.AppendLine();
            return sb.ToString();            
        }

        #endregion

        #region IEquatable Members
        /// <summary>
        /// Equality overrides from <see cref="System.Object"/>
        /// </summary>
        /// <param name="obj">The object to compare this with</param>
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FQuat4)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FQuat4 vector&&Equals(vector);
        }

        public static bool operator ==(FQuat4 target, FQuat4 other) => target.Equals(other);
        public static bool operator !=(FQuat4 target, FQuat4 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FQuat4"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FQuat4"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FQuat4 other)
        {
            return _data[0]==other._data[0]
                && _data[1]==other._data[1]
                && _data[2]==other._data[2]
                && _data[3]==other._data[3];
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FQuat4"/>
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
        public static FQuat4 Negate(FQuat4 a) => mul_scalar_quat4(-1.0, a);
        public static FQuat4 Scale(double s, FQuat4 a) => mul_scalar_quat4(s, a);
        public static FQuat4 Add(FQuat4 a, FQuat4 b) => add_quat4_quat4(a, b);
        public static FQuat4 Subtract(FQuat4 a, FQuat4 b) => sub_quat4_quat4(a, b);
        public static FQuat4 Product(FQuat4 a, FQuat4 b) => mul_quat4_quat4(a, b);
        public static double Dot(FQuat4 a, FQuat4 b) => inner_quat4_quat4(a, b);
        public static FVector3 Cross(FQuat4 a, FQuat4 b) => cross_quat4_quat4(a, b);
        public static FQuat4 Normalize(FQuat4 q) => quat4_normalize(q);
        public static FQuat4 Exp(FQuat4 q) => quat4_exp(q);
        #endregion

        #region Operators
        public static FQuat4 operator +(FQuat4 a, FQuat4 b) => Add(a, b);
        public static FQuat4 operator -(FQuat4 a) => Negate(a);
        public static FQuat4 operator -(FQuat4 a, FQuat4 b) => Subtract(a, b);
        public static FQuat4 operator *(double f, FQuat4 a) => Scale(f, a);
        public static FQuat4 operator *(FQuat4 a, double f) => Scale(f, a);
        public static FQuat4 operator /(FQuat4 a, double d) => Scale(1/d, a);
        public static FQuat4 operator *(FQuat4 a, FQuat4 b) => Product(a, b);
        public static double operator |(FQuat4 a, FQuat4 b) => Dot(a, b);
        public static FVector3 operator ^(FQuat4 a, FQuat4 b) => Cross(a, b);
        #endregion

        #region Fortran API
        const string libraryName = FortranMethods.libraryName;

        [DllImport(libraryName, EntryPoint = "quat4_zeros", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_zeros();
        [DllImport(libraryName, EntryPoint = "quat4_identity", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_identity();
        [DllImport(libraryName, EntryPoint = "quat4_values", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_values(double w, double x, double y, double z);        
        [DllImport(libraryName, EntryPoint = "quat4_scalar_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_scalar_vec3(double scalar, in FVector3 vector);
        [DllImport(libraryName, EntryPoint = "quat4_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_array(double[] values);
        [DllImport(libraryName, EntryPoint = "quat4_uniform", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_uniform(ref int seed);
        [DllImport(libraryName, EntryPoint = "norm_quat4", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double norm_quat4(in FQuat4 q);
        [DllImport(libraryName, EntryPoint = "quat4_axis_angle", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_axis_angle(in FVector3 axis, double angle);
        [DllImport(libraryName, EntryPoint = "quat4_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double quat4_scalar(in FQuat4 q);
        [DllImport(libraryName, EntryPoint = "quat4_vector", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 quat4_vector(in FQuat4 q);
        [DllImport(libraryName, EntryPoint = "add_quat4_quat4", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 add_quat4_quat4(in FQuat4 a, in FQuat4 b);
        [DllImport(libraryName, EntryPoint = "sub_quat4_quat4", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 sub_quat4_quat4(in FQuat4 a, in FQuat4 b);
        [DllImport(libraryName, EntryPoint = "mul_scalar_quat4", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 mul_scalar_quat4(double a, in FQuat4 b);
        [DllImport(libraryName, EntryPoint = "mul_quat4_quat4", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 mul_quat4_quat4(in FQuat4 a, in FQuat4 b);
        [DllImport(libraryName, EntryPoint = "inner_quat4_quat4", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double inner_quat4_quat4(in FQuat4 a, in FQuat4 b);
        [DllImport(libraryName, EntryPoint = "cross_quat4_quat4", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 cross_quat4_quat4(in FQuat4 a, in FQuat4 b);
        [DllImport(libraryName, EntryPoint = "quat4_to_matrix", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static  extern FMatrix3 quat4_to_matrix(in FQuat4 q, bool inverse);
        [DllImport(libraryName, EntryPoint = "quat4_from_matrix", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_from_matrix(in FMatrix3 rotation);
        [DllImport(libraryName, EntryPoint = "quat4_to_axis_angle", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void quat4_to_axis_angle(in FQuat4 q, [Out] out FVector3 axis, out double angle);
        [DllImport(libraryName, EntryPoint = "quat4_conjugate", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_conjugate(in FQuat4 q);
        [DllImport(libraryName, EntryPoint = "quat4_inverse", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_inverse(in FQuat4 q);
        [DllImport(libraryName, EntryPoint = "quat4_rotate_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 quat4_rotate_vec3(in FQuat4 q, in FVector3 vector, bool inverse);
        [DllImport(libraryName, EntryPoint = "quat4_rotate_diag", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 quat4_rotate_diag(in FQuat4 q, double[] diag, bool inverse);
        [DllImport(libraryName, EntryPoint = "quat4_normalize", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_normalize(in FQuat4 q);
        [DllImport(libraryName, EntryPoint = "quat4_exp", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FQuat4 quat4_exp(in FQuat4 q);

        #endregion

    }

}
