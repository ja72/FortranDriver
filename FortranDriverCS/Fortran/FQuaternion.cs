using System;
using System.Buffers;
using System.Runtime.InteropServices;
using System.Text;

namespace JA.Fortran
{
    /// <summary>
    /// Stores a quaternion object in scalar-vector convention.
    /// </summary>
    public unsafe class FQuaternion :
        IFormattable
    {
        private const int _size = 4;

        public static readonly double[] o_ = [0.0, 0.0, 0.0];
        public static readonly double[] i_ = [1.0, 0.0, 0.0];
        public static readonly double[] j_ = [0.0, 1.0, 0.0];
        public static readonly double[] k_ = [0.0, 0.0, 1.0];

        #region Factory
        internal FQuaternion(params double[] data)
        {
            Data = data ?? throw new ArgumentNullException(nameof(data));
        }
        public FQuaternion(double w, double x, double y, double z)
        {
            Data = [w, x, y, z];
        }
        public static FQuaternion FromRotationAxis(double[] axis, double angle)
        {
            double[] data = new double[4];
            FortranMethods.rotate_axis2quat(axis, angle, data);
            return new FQuaternion(data);
        }
        public static FQuaternion FromRotationMatrix(FMatrix R)
        {
            double[] data = new double[4];
            FortranMethods.rotate_mat2quat(R.Data, data);
            return new FQuaternion(data);
        }
        public static void FromRotationMatrixToAxisAngle(FMatrix R, out double[] axis, out double angle)
        {
            axis = new double[3];
            angle = 0;
            FortranMethods.rotate_mat2axis(R.Data, axis, ref angle);
        }
        public static FVector RotateVectorFromMatrix(FMatrix R, FVector vector)
        {
            double[] data = new double[3];
            FortranMethods.rotate_mat_vector(R.Data, vector.Data, data);
            return new FVector(data);
        }
        public static FMatrix FromAxisAngleToMatrix(double[] axis, double angle)
        {
            double[,] data = new double[3, 3];
            FortranMethods.rotate_axis2mat(axis, angle, data);
            return new FMatrix(data);
        }
        public static FVector RotateAxisAngleVector(double[] axis, double angle, double[] vector)
        {
            double[] data = new double[3];
            FortranMethods.rotate_axis_vector(axis, angle, vector, data);
            return new FVector(data);
        }

        private static int seed = Environment.TickCount;
        public static FQuaternion RandomUniform()
        {
            double[] data = new double[4];
            FortranMethods.quat_normal_01(ref seed, data);
            return new FQuaternion(data);
        }
        public static FQuaternion RandomRotation()
        {
            double[] data = new double[4];
            FortranMethods.rotate_normal_01(ref seed, data);
            return new FQuaternion(data);
        }

        public static FQuaternion FromRotationAxis(double ux, double uy, double uz, double angle)
        {
            return FromRotationAxis([ux, uy, uz], angle);
        }
        public static FQuaternion FromRotationX(double angle) => FromRotationAxis(1, 0, 0, angle);
        public static FQuaternion FromRotationY(double angle) => FromRotationAxis(0, 1, 0, angle);
        public static FQuaternion FromRotationZ(double angle) => FromRotationAxis(0, 0, 1, angle);
        public static FQuaternion Zero { get; } = new FQuaternion(0, 0, 0, 0);
        public static FQuaternion Identity { get; } = new FQuaternion(1, 0, 0, 0);
        public static FQuaternion UX { get; } = new FQuaternion(0, 1, 0, 0);
        public static FQuaternion UY { get; } = new FQuaternion(0, 0, 1, 0);
        public static FQuaternion UZ { get; } = new FQuaternion(0, 0, 0, 1);
        #endregion

        #region Properties
        public ref double this[Index index]
        {
            get
            {
                int i = index.GetOffset(Size);
                return ref Data[i - 1];
            }
        }

#pragma warning disable CA1822 // Mark members as static
        public int Size { get => _size; }
        public int Count { get => _size; }
#pragma warning restore CA1822 // Mark members as static
        internal double[] Data { get; }

        public Span<double> AsSpan()
        {
            fixed (double* ptr = &Data[0])
            {
                return new Span<double>(ptr, Data.Length);
            }
        }

        public double[] ToArray() => Data;

        public static implicit operator double[](FQuaternion a) => a.ToArray();
        public static explicit operator FQuaternion(double[] a) => new FQuaternion(a);

        public double Scalar()
        {
            double s = 0;
            FortranMethods.quat_scalar(Data, ref s);
            return s;
        }
        public FVector Vector()
        {
            double[] v = new double[3];
            FortranMethods.quat_vector(Data, v);
            return new FVector(v);
        }

        public FMatrix ToRotationMatrix(bool inverse = false)
        {
            double[,] R = new double[3, 3];
            if (inverse)
            {
                FortranMethods.rotate_quat2mat_inv(Data, R, true);
            }
            else
            {
                FortranMethods.rotate_quat2mat(Data, R);
            }
            return new FMatrix(R);
        }
        public void ToAxisAngle(out double[] axis, out double angle)
        {
            axis = new double[3];
            angle = 0;
            FortranMethods.rotate_quat2axis(Data, axis, ref angle);
        }
        #endregion

        #region Algebra
        public static double Dot(FQuaternion q1, FQuaternion q2)
        {
            double result = 0;
            FortranMethods.quat_dot(q1.Data, q2.Data, ref result);
            return result;
        }
        public static FVector Cross(FQuaternion q1, FQuaternion q2)
        {
            double[] result = new double[4];
            FortranMethods.quat_cross(q1.Data, q2.Data, result);
            return new FVector(result);
        }
        public static FQuaternion Negate(FQuaternion x)
            => Scale(-1, x);
        public static FQuaternion Add(FQuaternion q1, FQuaternion q2)
        {
            double[] data = new double[4];
            FortranMethods.quat_add(q1.Data, q2.Data, data);
            return new FQuaternion(data);
        }
        public static FQuaternion Subtract(FQuaternion q1, FQuaternion q2)
        {
            double[] data = new double[4];
            FortranMethods.quat_subtract(q1.Data, q2.Data, data);
            return new FQuaternion(data);
        }
        public static FQuaternion Scale(double x, FQuaternion q)
        {
            double[] data = new double[4];
            FortranMethods.quat_scale(x, q.Data, data);
            return new FQuaternion(data);
        }
        public static FQuaternion Multiply(FQuaternion q1, FQuaternion q2)
        {
            double[] data = new double[4];
            FortranMethods.quat_multiply(q1.Data, q2.Data, data);
            return new FQuaternion(data);
        }
        public static FQuaternion Multiply2(FQuaternion q1, FQuaternion q2)
        {
            double[] data = new double[4];
            FortranMethods.quat_multiply2(q1.Data, q2.Data, data);
            return new FQuaternion(data);
        }
        public static FQuaternion Exp(FQuaternion q)
        {
            double[] result = new double[4];
            FortranMethods.quat_exponentiate(q.Data, result);
            return new FQuaternion(result);
        }

        public FVector RotateVector(FVector vector, bool inverse = false)
        {
            double[] result = new double[vector.Count];
            if (inverse)
            {
                FortranMethods.rotate_quat_vector_inv(Data, vector.Data, result, true);
            }
            else
            {
                FortranMethods.rotate_quat_vector(Data, vector.Data, result);
            }
            return new FVector(result);
        }
        public FMatrix RotateDiagonal(double d1, double d2, double d3)
            => RotateDiagonal([d1, d2, d3]);
        public FMatrix RotateDiagonal(double[] diag)
        {
            double[,] result = new double[3, 3];
            FortranMethods.rotate_quat_diag2mat(Data, diag, result);
            return new FMatrix(result);
        }

        public FQuaternion Conjugate()
        {
            double[] result = new double[4];
            FortranMethods.quat_conjugate(Data, result);
            return new FQuaternion(result);
        }
        public FQuaternion Inverse()
        {
            double[] result = new double[4];
            FortranMethods.quat_inverse(Data, result);
            return new FQuaternion(result);
        }

        public double Norm()
        {
            return FortranMethods.quat_norm(Data);
        }

        public static FQuaternion Normalize(FQuaternion q)
        {
            double[] result = new double[4];
            FortranMethods.rotate_normalize(q.Data, result);
            return new FQuaternion(result);
        }

        #endregion

        #region Operators
        public static FQuaternion operator +(FQuaternion q)
            => q;
        public static FQuaternion operator +(FQuaternion q1, FQuaternion b)
            => Add(q1, b);
        public static FQuaternion operator -(FQuaternion q)
            => Negate(q);
        public static FQuaternion operator -(FQuaternion q1, FQuaternion q2)
            => Subtract(q1, q2);
        public static FQuaternion operator *(double x, FQuaternion q)
            => Scale(x, q);
        public static FQuaternion operator *(FQuaternion q, double x)
            => Scale(x, q);
        public static FQuaternion operator /(FQuaternion q, double x)
            => Scale(1 / x, q);
        public static FQuaternion operator *(FQuaternion q1, FQuaternion q2)
            => Multiply(q1, q2);
        public static FQuaternion operator ^(double b, FQuaternion q)
            => Exp(q * Math.Log(b));
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";

        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            const int width = HelperFunctions.DefaultColumnWidth;
            StringBuilder sb = new StringBuilder();
            int n = Data.Length;
            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                var f_val = Math.Round(Data[i], HelperFunctions.RoundDigits);
                string text = f_val.ToString(formatting, formatProvider);
                text = text.PadLeft(width);
                if (text.Length > width)
                {
#pragma warning disable IDE0057 // Use range operator
                    text = $"{text.Substring(0, width - 1)}…";
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

        #region Testing
        public static void TestFortranQuaternion()
        {
            FortranMethods.call_quat_test_all();
        }
        public static void TestFQuaternion()
        {
            const double pi = Math.PI;
            const double deg = pi / 180;

            //FortranMethods.call_quat_test_all();

            {
                Console.WriteLine("Test Scalar Functions =================================");
                double cos_th = 0.5;
                double th = FortranMethods.acos_scalar(cos_th);
                double th_deg = FortranMethods.radians_to_degrees(th);
                double th_rad = FortranMethods.degrees_to_radians(th_deg);

                Console.WriteLine($"ACOS({cos_th}) => {th} rad => {th_deg} deg => {th_rad} rad");
                Console.WriteLine();

            }
            {
                Console.WriteLine("Test Rotation Algebra =================================");
                FQuaternion q1 = RandomUniform();

                Console.WriteLine($"Random Quaternion q1= \n{q1}");
                Console.WriteLine($"Norm(q1) = {q1.Norm()}\n");

                double q_scalar = q1.Scalar();
                FVector q_vector = q1.Vector();

                Console.WriteLine($"Scalar(q1) = {q_scalar}\n");
                Console.WriteLine($"Vector(q1) = \n{q_vector}");

                FQuaternion q2 = RandomUniform();

                Console.WriteLine($"Random Quaternion q2= \n{q2}");
                Console.WriteLine($"Norm(q2) = {q2.Norm()}\n");

                double q_dot = Dot(q1, q2);
                var q_cross = Cross(q1, q2);
                Console.WriteLine($"DOT(q1,q2) = {q_dot}");
                Console.WriteLine($"CROSS(q1,q2) = \n{q_cross}");

                Console.WriteLine($"Conjugate Quaternion, CONJ(q1) = \n{q1.Conjugate()}");
                Console.WriteLine($"Inverse Quaternion, INV(q1) = \n{q1.Inverse()}");
                Console.WriteLine($"Inverse Quaternion Check, INV(q1)*q1 = \n{q1.Inverse() * q1}");

                FMatrix R = q1.ToRotationMatrix();
                FMatrix R_inv = q1.ToRotationMatrix(true);

                Console.WriteLine($"Rotation Matrix=\n{FMatrix.Round(R, 6)}");
                Console.WriteLine($"Inverse Rotation Matrix=\n{FMatrix.Round(R_inv, 6)}");

                FMatrix eye3 = FMatrix.Round(R_inv * R, 6);
                Console.WriteLine($"Check that product is identity matrix\n{eye3}");

            }
            {
                Console.WriteLine("Test Rotation Constructors ============================");
                double[] q_arr = new double[4];
                FQuaternion q_rnd = RandomUniform();
                Console.WriteLine($"Uniform Quaternion = \n{q_rnd}");
                Console.WriteLine($"Norm = {q_rnd.Norm()}\n");
                FQuaternion q_norm = Normalize(q_rnd);
                Console.WriteLine($"Normalized Quaternion = \n{q_norm}");
                Console.WriteLine($"Norm = {q_norm.Norm()}\n");
                FQuaternion q_rot = RandomRotation();
                Console.WriteLine($"Random Rotation = \n{q_rot}");
                Console.WriteLine($"Norm = {q_rot.Norm()}\n");

                double[] axis = FVector.RandomMinMax(3);
                double angle = 30 * deg;
                Console.WriteLine($"AXIS=({axis.ToListString()}), ANGLE=({angle / deg} deg)");

                FQuaternion q = FromRotationAxis(axis, angle);
                Console.WriteLine($"Quaternion = \n{q}");
                Console.WriteLine($"Norm = {q.Norm()}\n");

                FMatrix R_test = FromAxisAngleToMatrix(axis, angle);
                Console.WriteLine($"Axis angle to Rotation Matrix=\n{FMatrix.Round(R_test, 6)}");


                FMatrix R = q.ToRotationMatrix();
                FQuaternion q_test = FromRotationMatrix(R);
                Console.WriteLine($"Quat to Rotation Matrix=\n{FMatrix.Round(R, 6)}");

            }
            {
                Console.WriteLine("Test Rotation Converasion =============================");

                FQuaternion q = RandomRotation();
                Console.WriteLine($"Rotation Quaternion = \n{q}");
                FMatrix R = q.ToRotationMatrix();

                q.ToAxisAngle(out var axis, out var angle);

                FQuaternion q_axis = FromRotationAxis(axis, angle);
                Console.WriteLine($"Quaternion From Axis/Angle = \n{q_axis}");

                FVector v = FVector.RandomUniform(3);
                Console.WriteLine($"Random Vector = \n{v}");
                FVector v_rot = q.RotateVector(v);
                Console.WriteLine($"Rotated Vector = \n{v_rot}");


                Console.WriteLine($"Rotation Matrix=\n{FMatrix.Round(R, 6)}");
                FMatrix R_axis = FromAxisAngleToMatrix(axis, angle);
                Console.WriteLine($"Rotation Matrix From Axis/Angle = \n{FMatrix.Round(R_axis, 6)}");

                FromRotationMatrixToAxisAngle(R_axis, out axis, out angle);

                Console.WriteLine($"From Quaternion Axis = [{axis.ToListString()}]");
                Console.WriteLine($"From Quaternion Angle = {angle}");
                Console.WriteLine($"From Matrix Axis = [{axis.ToListString()}]");
                Console.WriteLine($"From Matrix Angle = {angle}");
                Console.WriteLine();
            }
            {
                Console.WriteLine("Test MMOI Rotations ===================================");
                double dx = 1, dy = 2, dz = 3;
                double[] I_diag = [dy * dy + dz * dz, dx * dx + dz * dz, dx * dx + dy * dy];

                Console.WriteLine($"Diagonal MMOI = {I_diag.ToListString()}\n");


                FQuaternion q_rnd = RandomRotation();

                //NativeQuaternion q_rnd = new NativeQuaternion([0.550882, -0.7006629, 0.4458437, -0.08260677]);

                Console.WriteLine($"Random Quaternion=\n{q_rnd}");
                Console.WriteLine($"Norm = {q_rnd.Norm()}\n");

                FMatrix R = q_rnd.ToRotationMatrix();
                FMatrix R_inv = q_rnd.ToRotationMatrix(true);

                Console.WriteLine($"Rotation Matrix=\n{R}");

                FMatrix I_mmoi = q_rnd.RotateDiagonal(I_diag);

                Console.WriteLine($"I_mmoi = \n{I_mmoi}");

                FMatrix I_check = R * FMatrix.Diagonal(I_diag) * R_inv;

                Console.WriteLine($"I_check = \n{I_check}");

                double max_err = (I_mmoi - I_check).ToArray().Max((x) => Math.Abs(x));

                Console.WriteLine($"Maximum Error = {max_err}");
                Console.WriteLine();
            }
        }

        #endregion

    }

}
