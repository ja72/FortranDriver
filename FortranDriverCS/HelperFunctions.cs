using System.Diagnostics.Contracts;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;

namespace FortranDriver
{
    public static class HelperFunctions
    {
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
                result[i] = init(i+1);
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
                    result[i, j] =  defaultValue;
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
                    result[i, j] = init(j+1, i+1);
                }
            }
            return result;
        }

        public static bool ValueEquals<T>(this T[] first, T[] second) where T : IEquatable<T>
            => Enumerable.SequenceEqual(first, second);

        public static void ShowInConsole(this double[] A, string title, int width = 11, string formatting = "g4")
        {
            Console.WriteLine(title);
            Console.WriteLine(A.ToFixedColumnString(formatting, width: width));
        }

        public static void ShowInConsole(this double[,] A, string title, int width = 11, string formatting = "g4")
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
        public static string ToFixedColumnString<T>(this T[] data, string formatting = null, int width = 11)
            => ToFixedColumnString<T>(data, formatting, null, width);
        public static string ToFixedColumnString<T>(this T[] data, string formatting = null, IFormatProvider provider = null, int width = 11)
        {
            StringBuilder sb = new StringBuilder();
            int n = data.Length;
            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                var obj = data[i];
                string text = obj.ToString<T>(formatting, provider);
                text  = text.PadLeft(width);
                if (text.Length>width)
                {
                    text = $"{text.Substring(0, width-1)}…";
                }
                sb.Append($" {text}");
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }
        public static string ToFixedColumnString<T>(this T[,] data, string formatting = null, int width = 11)
            => ToFixedColumnString<T>(data, formatting, null, width);
        public static string ToFixedColumnString<T>(this T[,] data, string formatting = null, IFormatProvider provider = null, int width = 11)
        {
            StringBuilder sb = new StringBuilder();
            int n = data.GetLength(1), m = data.GetLength(0);
            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                for (int j = 0; j < m; j++)
                {
                    var obj = data[j, i];
                    string text = obj.ToString<T>(formatting, provider);
                    text  = text.PadLeft(width);
                    if (text.Length>width)
                    {
                        text = $"{text.Substring(0, width-1)}…";
                    }
                    sb.Append($" {text}");
                }
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }

    }
}
