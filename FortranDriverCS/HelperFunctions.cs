using System;
using System.Diagnostics.Contracts;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;

namespace JA
{
    public static class HelperFunctions
    {
        public const int DefaultColumnWidth = 11;
        public static int RoundDigits { get; } = 11;
        public static double[] LinearSpace(int size, double xmin, double xmax)
        {
            double h = (xmax - xmin)/size;
            double[] x = new double[size+1];
            for (int i = 0; i <= size; i++)
            {
                x[i] = xmin + i * h;
            }
            return x;
        }
        public static T[] BuildArray<T>(int size, T defaultValue)
        {
            var result = new T[size];
            for (int i = 0; i < size; i++)
            {
                result[i] = defaultValue;
            }
            return result;
        }
        public static T[] BuildArray<T>(int size, Func<int, T> init)
        {
            Contract.Requires(init != null);
            var result = new T[size];
            for (int i = 0; i < size; i++)
            {
                result[i] = init(i + 1);
            }
            return result;
        }
        public static T[,] BuildArray<T>(int rows, int columns, T defaultValue)
        {
            var result = new T[columns, rows];
            for (int i = 0; i < columns; i++)
            {
                for (int j = 0; j < rows; j++)
                {
                    result[i, j] = defaultValue;
                }
            }
            return result;
        }
        public static T[,] BuildArray<T>(int rows, int columns, Func<int, int, T> init = null)
        {
            Contract.Requires(init != null);
            var result = new T[columns, rows];
            for (int i = 0; i < columns; i++)
            {
                for (int j = 0; j < rows; j++)
                {
                    result[i, j] = init(j + 1, i + 1);
                }
            }
            return result;
        }

        public static bool ValueEquals<T>(this T[] first, T[] second) where T : IEquatable<T>
            => first.SequenceEqual(second);

        public static void ShowInConsole(this double[] A, string title, int width = HelperFunctions.DefaultColumnWidth, string formatting = "g4")
        {
            Console.WriteLine(title);
            Console.WriteLine(A.ToFixedColumnString(formatting, width: width));
        }

        public static void ShowInConsole(this double[,] A, string title, int width = HelperFunctions.DefaultColumnWidth, string formatting = "g4")
        {
            Console.WriteLine(title);
            Console.WriteLine(A.ToFixedColumnString(formatting, width: width));
        }
        //public static string DefaultFormat { get; set; } = "g6";
        public static string ToString<T>(this T obj)
        {
            return obj switch
            {
                _ when obj is int i => i.ToString("g"),
                _ when obj is float t => t.ToString("g"),
                _ when obj is double x => Math.Round(x, 12).ToString("g"),
                _ when obj is IFormattable f => f.ToString("g"),
                _ => obj.ToString(),
            };
        }
        public static string ToString<T>(this T obj, string formatting, IFormatProvider provider = null)
        {
            provider ??= CultureInfo.CurrentCulture.NumberFormat;
            return obj switch
            {
                _ when obj is int i => i.ToString(formatting, provider),
                _ when obj is float t => t.ToString(formatting, provider),
                _ when obj is double x => Math.Round(x, 12).ToString(formatting, provider),
                _ when obj is IFormattable f => f.ToString(formatting, provider),
                _ => obj.ToString(),
            };
        }

        public static string ToListString<T>(this T[] array)
        {
            return string.Join(",", array.Select((x) => x.ToString<T>()));
        }
        public static string ToListString<T>(this T[] array, string formatting, string separator = ",") where T : IFormattable
        {
            return string.Join(separator, array.Select((x) => x.ToString<T>(formatting, null)));
        }
        public static string ToFixedColumnString<T>(this T[] data, string formatting, int width)
            => data.ToFixedColumnString(formatting, null, width);
        public static string ToFixedColumnString<T>(this T[] data, string formatting, IFormatProvider provider, int width)
        {
            StringBuilder sb = new StringBuilder();
            int n = data.Length;
            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                var obj = data[i];
                string text = obj.ToString(formatting, provider);
                text = text.PadLeft(width);
                if (text.Length > width)
                {
#pragma warning disable IDE0057 // Use range operator
                    text = $"{text.Substring(0, width - 1)}…";
#pragma warning restore IDE0057 // Use range operator
                }
                sb.Append($" {text}");
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }
        public static string ToFixedColumnString<T>(this T[,] data, string formatting = null, int width = HelperFunctions.DefaultColumnWidth)
            => data.ToFixedColumnString(formatting, null, width);
        public static string ToFixedColumnString<T>(this T[,] data, string formatting = null, IFormatProvider provider = null, int width = HelperFunctions.DefaultColumnWidth)
        {
            StringBuilder sb = new StringBuilder();
            int n = data.GetLength(1), m = data.GetLength(0);
            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                for (int j = 0; j < m; j++)
                {
                    var obj = data[j, i];
                    string text = obj.ToString(formatting, provider);
                    text = text.PadLeft(width);
                    if (text.Length > width)
                    {
#pragma warning disable IDE0057 // Use range operator
                        text = $"{text.Substring(0, width - 1)}…";
#pragma warning restore IDE0057 // Use range operator
                    }
                    sb.Append($" {text}");
                }
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }
        public static double Quantize(this double x, double x_max)
            => Quantize(x, 0, Math.Abs(x_max));
        public static double Quantize(this double x, double x_min, double x_max, int divisions = 10)
        {
            int sign = Math.Sign(x_max-x_min);
            (x_min, x_max) = (Math.Min(x_min, x_max), Math.Max(x_min, x_max) );
            double span = x_max  - x_min;
            int exp = (int)Math.Floor(Math.Log10(span));
            double div = Math.Pow(10, exp);
            double f = x/div;
            // x = f * 10^exp
            f = Math.Round(divisions * f)/ divisions;
            return f*div;
        }

    }
}
