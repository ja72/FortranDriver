using System;
using System.Buffers;
using System.Runtime.InteropServices;
using System.Text;

namespace FortranDriver
{
    /// <summary>
    /// Stores a quaternion object in scalar-vector convention.
    /// </summary>
    public unsafe class NativeQuaternion :
        IFormattable
    {
        public static readonly double[] o_ = [0.0, 0.0, 0.0];
        public static readonly double[] i_ = [1.0, 0.0, 0.0];
        public static readonly double[] j_ = [0.0, 1.0, 0.0];
        public static readonly double[] k_ = [0.0, 0.0, 1.0];

        #region Factory
        internal NativeQuaternion(params double[] data)
        {
            this.Data=data??throw new ArgumentNullException(nameof(data));
        }
        public NativeQuaternion(double w, double x, double y, double z)
        {
            this.Data = [w, x, y, z];
        }
        public static NativeQuaternion FromRotationAxis(double[] axis, double angle)
        {
            double[] data = new double[4];
            FortranMethods.rotation_axis2quat(axis, angle, data);
            return new NativeQuaternion(data);
        }
        public static NativeQuaternion FromRotationMatrix(NativeMatrix R)
        {
            double[] data = new double[4];
            FortranMethods.rotation_mat2quat(R.Data, data);
            return new NativeQuaternion(data);
        }
        public static void FromRotationMatrixToAxisAngle(NativeMatrix R, out double[] axis, out double angle)
        {
            axis = new double[3];
            angle = 0;
            FortranMethods.rotation_mat2axis(R.Data, axis, ref angle);
        }
        public static NativeVector RotateVectorFromMatrix(NativeMatrix R, NativeVector vector)
        {
            double[] data = new double[3];
            FortranMethods.rotation_mat_vector(R.Data, vector.Data, data);
            return new NativeVector(data);
        }
        public static NativeMatrix FromAxisAngleToMatrix(double[] axis, double angle)
        {
            double[,] data = new double[3, 3];
            FortranMethods.rotation_axis2mat(axis, angle, data);
            return new NativeMatrix(data);
        }
        public static NativeVector RotateAxisAngleVector(double[] axis, double angle, double[] vector)
        {
            double[] data = new double[3];
            FortranMethods.rotation_axis_vector(axis, angle, vector, data);
            return new NativeVector(data);
        }

        private static int seed = Environment.TickCount;
        public static NativeQuaternion RandomUniform()
        {
            double[] data = new double[4];
            FortranMethods.q8_normal_01(ref seed, data);
            return new NativeQuaternion(data);
        }
        public static NativeQuaternion RandomRotation()
        {
            double[] data = new double[4];
            FortranMethods.rotate_normal_01(ref seed, data);
            return new NativeQuaternion(data);
        }

        public static NativeQuaternion FromRotationAxis(double ux, double uy, double uz, double angle)
        {
            return FromRotationAxis([ux, uy, uz], angle);
        }
        public static NativeQuaternion FromRotationX(double angle) => FromRotationAxis(1, 0, 0, angle);
        public static NativeQuaternion FromRotationY(double angle) => FromRotationAxis(0, 1, 0, angle);
        public static NativeQuaternion FromRotationZ(double angle) => FromRotationAxis(0, 0, 1, angle);
        public static NativeQuaternion Zero { get; } = new NativeQuaternion(0, 0, 0, 0);
        public static NativeQuaternion Identity { get; } = new NativeQuaternion(1, 0, 0, 0);
        public static NativeQuaternion UX { get; } = new NativeQuaternion(0, 1, 0, 0);
        public static NativeQuaternion UY { get; } = new NativeQuaternion(0, 0, 1, 0);
        public static NativeQuaternion UZ { get; } = new NativeQuaternion(0, 0, 0, 1);
        #endregion

        #region Properties
        public ref double this[Index index]
        {
            get
            {
                int i = index.GetOffset(Size);
                return ref Data[i-1];
            }
        }

        public int Size { get => 4; }
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

        public static implicit operator double[](NativeQuaternion a) => a.ToArray();
        public static explicit operator NativeQuaternion(double[] a) => new NativeQuaternion(a);

        public double Scalar()
        {
            double s = 0;
            FortranMethods.q8_scalar(Data, ref s);
            return s;
        }
        public NativeVector Vector()
        {
            double[] v = new double[3];
            FortranMethods.q8_vector(Data, v);
            return new NativeVector(v);
        }

        public NativeMatrix ToRotationMatrix(bool inverse = false)
        {
            double[,] R = new double[3, 3];
            if (inverse)
            {
                FortranMethods.rotation_quat2mat_inv(Data, R, true);
            }
            else
            {
                FortranMethods.rotation_quat2mat(Data, R);
            }
            return new NativeMatrix(R);
        }
        public void ToAxisAngle(out double[] axis, out double angle)
        {
            axis = new double[3];
            angle = 0;
            FortranMethods.rotation_quat2axis(Data, axis, ref angle);
        }
        #endregion

        #region Algebra
        public static double Dot(NativeQuaternion q1, NativeQuaternion q2)
        {
            double result = 0;
            FortranMethods.q8_dot(q1.Data, q2.Data, ref result);
            return result;
        }
        public static NativeVector Cross(NativeQuaternion q1, NativeQuaternion q2)
        {
            double[] result = new double[4];
            FortranMethods.q8_cross(q1.Data, q2.Data, result);
            return new NativeVector(result);
        }
        public static NativeQuaternion Negate(NativeQuaternion x)
            => Scale(-1, x);
        public static NativeQuaternion Add(NativeQuaternion q1, NativeQuaternion q2)
        {
            double[] data = new double[4];
            FortranMethods.q8_add(q1.Data, q2.Data, data);
            return new NativeQuaternion(data);
        }
        public static NativeQuaternion Subtract(NativeQuaternion q1, NativeQuaternion q2)
        {
            double[] data = new double[4];
            FortranMethods.q8_subtract(q1.Data, q2.Data, data);
            return new NativeQuaternion(data);
        }
        public static NativeQuaternion Scale(double x, NativeQuaternion q)
        {
            double[] data = new double[4];
            FortranMethods.q8_scale(x, q.Data, data);
            return new NativeQuaternion(data);
        }
        public static NativeQuaternion Multiply(NativeQuaternion q1, NativeQuaternion q2)
        {
            double[] data = new double[4];
            FortranMethods.q8_multiply(q1.Data, q2.Data, data);
            return new NativeQuaternion(data);
        }
        public static NativeQuaternion Multiply2(NativeQuaternion q1, NativeQuaternion q2)
        {
            double[] data = new double[4];
            FortranMethods.q8_multiply2(q1.Data, q2.Data, data);
            return new NativeQuaternion(data);
        }
        public static NativeQuaternion Exp(NativeQuaternion q)
        {
            double[] result = new double[4];
            FortranMethods.q8_exponentiate(q.Data, result);
            return new NativeQuaternion(result);
        }

        public NativeVector RotateVector(NativeVector vector, bool inverse = false)
        {
            double[] result = new double[vector.Count];
            if (inverse)
            {
                FortranMethods.rotation_quat_vector_inv(Data, vector.Data, result, true);
            }
            else
            {
                FortranMethods.rotation_quat_vector(Data, vector.Data, result);
            }
            return new NativeVector(result);
        }
        public NativeMatrix RotateDiagonal(double d1, double d2, double d3)
            => RotateDiagonal([d1, d2, d3]);
        public NativeMatrix RotateDiagonal(double[] diag)
        {
            if (diag.Length<3)
            {
                var temp = new double[3];
                Array.Copy(diag, temp, diag.Length);
                diag = temp;
            }
            else if (diag.Length>3)
            {
                var temp = new double[3];
                Array.Copy(diag, temp, temp.Length);
                diag = temp;
            }
            double[,] result = new double[3, 3];
            FortranMethods.rotation_diag2mat(Data, diag, result);
            return new NativeMatrix(result);
        }

        public NativeQuaternion Conjugate()
        {
            double[] result = new double[4];
            FortranMethods.q8_conjugate(Data, result);
            return new NativeQuaternion(result);
        }
        public NativeQuaternion Inverse()
        {
            double[] result = new double[4];
            FortranMethods.q8_inverse(Data, result);
            return new NativeQuaternion(result);
        }

        public double Norm()
        {
            return FortranMethods.q8_norm(Data);
        }

        public static NativeQuaternion Normalize(NativeQuaternion q)
        {
            double[] result = new double[4];
            FortranMethods.rotation_normalize(q.Data, result);
            return new NativeQuaternion(result);
        }

        #endregion

        #region Operators
        public static NativeQuaternion operator +(NativeQuaternion q)
            => q;
        public static NativeQuaternion operator +(NativeQuaternion q1, NativeQuaternion b)
            => Add(q1, b);
        public static NativeQuaternion operator -(NativeQuaternion q)
            => Negate(q);
        public static NativeQuaternion operator -(NativeQuaternion q1, NativeQuaternion q2)
            => Subtract(q1, q2);
        public static NativeQuaternion operator *(double x, NativeQuaternion q)
            => Scale(x, q);
        public static NativeQuaternion operator *(NativeQuaternion q, double x)
            => Scale(x, q);
        public static NativeQuaternion operator /(NativeQuaternion q, double x)
            => Scale(1 / x, q);
        public static NativeQuaternion operator *(NativeQuaternion q1, NativeQuaternion q2)
            => Multiply(q1, q2);
        public static NativeQuaternion operator ^(double b, NativeQuaternion q)
            => Exp(q*Math.Log(b));
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";

        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            const int width = 11;
            StringBuilder sb = new StringBuilder();
            int n = Data.Length;
            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                var obj = Data[i];
                string text = obj.ToString(formatting, formatProvider);
                text  = text.PadLeft(width);
                if (text.Length>width)
                {
                    text = $"{text.Substring(0, width-1)}…";
                }
                sb.Append($" {text}");
                sb.AppendLine(" |");
                if (i==0)
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

        public static void TestNativeQuaternion()
        {
            const double pi = Math.PI;
            const double deg = pi/180;            

            //FortranMethods.quat_array_test();

            { 
                Console.WriteLine("Test Scalar Functions =================================");
                double cos_th = 0.5;
                double th = FortranMethods.r8_acos(cos_th);
                double th_deg = FortranMethods.radians_to_degrees(th);
                double th_rad = FortranMethods.degrees_to_radians(th_deg);

                Console.WriteLine($"ACOS({cos_th}) => {th} rad => {th_deg} deg => {th_rad} rad");
                Console.WriteLine();

            }
            {
                Console.WriteLine("Test Rotation Algebra =================================");
                NativeQuaternion q1 = NativeQuaternion.RandomUniform();

                Console.WriteLine($"Random Quaternion q1= \n{q1}");
                Console.WriteLine($"Norm(q1) = {q1.Norm()}\n");

                double q_scalar = q1.Scalar();
                NativeVector q_vector = q1.Vector();

                Console.WriteLine($"Scalar(q1) = {q_scalar}\n");
                Console.WriteLine($"Vector(q1) = \n{q_vector}");

                NativeQuaternion q2 = NativeQuaternion.RandomUniform();

                Console.WriteLine($"Random Quaternion q2= \n{q2}");
                Console.WriteLine($"Norm(q2) = {q2.Norm()}\n");

                double q_dot = NativeQuaternion.Dot(q1, q2);
                var q_cross = NativeQuaternion.Cross(q1, q2);
                Console.WriteLine($"DOT(q1,q2) = {q_dot}");
                Console.WriteLine($"CROSS(q1,q2) = \n{q_cross}");

                Console.WriteLine($"Conjugate Quaternion, CONJ(q1) = \n{q1.Conjugate()}");
                Console.WriteLine($"Inverse Quaternion, INV(q1) = \n{q1.Inverse()}");
                Console.WriteLine($"Inverse Quaternion Check, INV(q1)*q1 = \n{q1.Inverse() * q1}");

                NativeMatrix R = q1.ToRotationMatrix();
                NativeMatrix R_inv = q1.ToRotationMatrix(true);

                Console.WriteLine($"Rotation Matrix=\n{NativeMatrix.Round(R,6)}");
                Console.WriteLine($"Inverse Rotation Matrix=\n{NativeMatrix.Round(R_inv,6)}");

                NativeMatrix eye3 = NativeMatrix.Round( R_inv * R, 6);
                Console.WriteLine($"Check that product is identity matrix\n{eye3}");

            }
            {
                Console.WriteLine("Test Rotation Constructors ============================");
                double[] q_arr = new double[4];
                NativeQuaternion q_rnd = NativeQuaternion.RandomUniform();
                Console.WriteLine($"Uniform Quaternion = \n{q_rnd}");
                Console.WriteLine($"Norm = {q_rnd.Norm()}\n");
                NativeQuaternion q_norm = NativeQuaternion.Normalize(q_rnd);
                Console.WriteLine($"Normalized Quaternion = \n{q_norm}");
                Console.WriteLine($"Norm = {q_norm.Norm()}\n");
                NativeQuaternion q_rot = NativeQuaternion.RandomRotation();
                Console.WriteLine($"Random Rotation = \n{q_rot}");
                Console.WriteLine($"Norm = {q_rot.Norm()}\n");

                double[] axis = NativeVector.RandomMinMax(3);
                double angle = 30*deg;
                Console.WriteLine($"AXIS=({axis.ToListString()}), ANGLE=({angle/deg} deg)");

                NativeQuaternion q = FromRotationAxis(axis, angle);
                Console.WriteLine($"Quaternion = \n{q}");
                Console.WriteLine($"Norm = {q.Norm()}\n");

                NativeMatrix R_test = NativeQuaternion.FromAxisAngleToMatrix(axis, angle);
                Console.WriteLine($"Axis angle to Rotation Matrix=\n{NativeMatrix.Round(R_test,6)}");


                NativeMatrix R = q.ToRotationMatrix();
                NativeQuaternion q_test = NativeQuaternion.FromRotationMatrix(R);
                Console.WriteLine($"Quat to Rotation Matrix=\n{NativeMatrix.Round(R,6)}");

            }
            {
                Console.WriteLine("Test Rotation Converasion =============================");

                NativeQuaternion q = NativeQuaternion.RandomRotation();
                Console.WriteLine($"Rotation Quaternion = \n{q}");
                NativeMatrix R = q.ToRotationMatrix();

                q.ToAxisAngle(out var axis, out var angle);

                NativeQuaternion q_axis = NativeQuaternion.FromRotationAxis(axis, angle);
                Console.WriteLine($"Quaternion From Axis/Angle = \n{q_axis}");

                NativeVector v = NativeVector.RandomUniform(3);
                Console.WriteLine($"Random Vector = \n{v}");
                NativeVector v_rot = q.RotateVector(v);
                Console.WriteLine($"Rotated Vector = \n{v_rot}");


                Console.WriteLine($"Rotation Matrix=\n{NativeMatrix.Round(R,6)}");
                NativeMatrix R_axis = NativeQuaternion.FromAxisAngleToMatrix(axis, angle);
                Console.WriteLine($"Rotation Matrix From Axis/Angle = \n{NativeMatrix.Round(R_axis,6)}");

                NativeQuaternion.FromRotationMatrixToAxisAngle(R_axis, out axis, out angle);

                Console.WriteLine($"From Quaternion Axis = [{axis.ToListString()}]");
                Console.WriteLine($"From Quaternion Angle = {angle}");
                Console.WriteLine($"From Matrix Axis = [{axis.ToListString()}]");
                Console.WriteLine($"From Matrix Angle = {angle}");
                Console.WriteLine();
            }
            {
                Console.WriteLine("Test MMOI Rotations ===================================");
                double dx = 1, dy = 2, dz = 3;
                double[] I_diag = [dy*dy+dz*dz, dx*dx+dz*dz, dx*dx+dy*dy];

                Console.WriteLine($"Diagonal MMOI = {I_diag.ToListString()}\n");


                NativeQuaternion q_rnd = NativeQuaternion.RandomRotation();

                //NativeQuaternion q_rnd = new NativeQuaternion([0.550882, -0.7006629, 0.4458437, -0.08260677]);

                Console.WriteLine($"Random Quaternion=\n{q_rnd}");
                Console.WriteLine($"Norm = {q_rnd.Norm()}\n");

                NativeMatrix R = q_rnd.ToRotationMatrix();
                NativeMatrix R_inv = q_rnd.ToRotationMatrix(true);

                Console.WriteLine($"Rotation Matrix=\n{R}");

                NativeMatrix I_mmoi = q_rnd.RotateDiagonal(I_diag);

                Console.WriteLine($"I_mmoi = \n{I_mmoi}");

                NativeMatrix I_check = R * NativeMatrix.Diagonal(I_diag) * R_inv;

                Console.WriteLine($"I_check = \n{I_check}");

                double max_err = (I_mmoi - I_check).ToArray().Max((x) => Math.Abs(x));

                Console.WriteLine($"Maximum Error = {max_err}");
                Console.WriteLine();
            }
        }

        #endregion


    }

}
