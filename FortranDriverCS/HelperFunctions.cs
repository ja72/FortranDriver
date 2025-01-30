using System;
using System.Diagnostics.Contracts;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;
using System.Linq;

namespace JA
{
    public static class HelperFunctions
    {
        public const int DefaultColumnWidth = 11;
        public static int RoundDigits { get; } = 11;
        public static string DefaultFormatting { get; set; } = "g6";
        public static Random RNG { get; } = new Random();
        public static T[] BuildArray<T>(int size, T defaultValue)
        {
            var result = new T[size];
            for (int i = 0; i<size; i++)
            {
                result[i]=defaultValue;
            }
            return result;
        }
        public static T[] BuildArray<T>(int size, Func<int, T> init)
        {
            Contract.Requires(init!=null);
            var result = new T[size];
            for (int i = 0; i<size; i++)
            {
                result[i]=init(i+1);
            }
            return result;
        }
        public static T[,] BuildArray<T>(int rows, int columns, T defaultValue)
        {
            var result = new T[columns, rows];
            for (int i = 0; i<columns; i++)
            {
                for (int j = 0; j<rows; j++)
                {
                    result[i, j]=defaultValue;
                }
            }
            return result;
        }
        public static T[,] BuildArray<T>(int rows, int columns, Func<int, int, T> init = null)
        {
            Contract.Requires(init!=null);
            var result = new T[columns, rows];
            for (int i = 0; i<columns; i++)
            {
                for (int j = 0; j<rows; j++)
                {
                    result[i, j]=init(j+1, i+1);
                }
            }
            return result;
        }

        public static bool ValueEquals<T>(this T[] first, T[] second) where T : IEquatable<T>
            => first.SequenceEqual(second);

        public static void ShowInConsole(this double[] A, string title, int width = HelperFunctions.DefaultColumnWidth, string formatting = null)
        {
            formatting??=DefaultFormatting;
            Console.WriteLine(title);
            Console.WriteLine(A.ToFixedColumnString(width, formatting));
        }

        public static void ShowInConsole(this double[,] A, string title, int width = HelperFunctions.DefaultColumnWidth, string formatting = null)
        {
            formatting??=DefaultFormatting;
            Console.WriteLine(title);
            Console.WriteLine(A.ToFixedColumnString(formatting, width: width));
        }
        public static string ToString<T>(this T obj)
        {
            return obj switch
            {
                _ when obj is int i => i.ToString("g"),
                _ when obj is float t => t.ToString(DefaultFormatting),
                _ when obj is double x => Math.Round(x, 12).ToString(DefaultFormatting),
                _ when obj is IFormattable f => f.ToString(DefaultFormatting, CultureInfo.CurrentCulture.NumberFormat),
                _ => obj.ToString(),
            };
        }
        public static string ToString<T>(this T obj, string formatting, IFormatProvider provider = null)
        {
            provider??=CultureInfo.CurrentCulture.NumberFormat;
            return obj switch
            {
                _ when obj is int i => i.ToString(formatting, provider),
                _ when obj is float t => t.ToString(formatting, provider),
                _ when obj is double x => Math.Round(x, 12).ToString(formatting, provider),
                _ when obj is IFormattable f => f.ToString(formatting, provider),
                _ => obj.ToString(),
            };
        }

        public static string ToListString<T>(this IList<T> array)
        {
            return string.Join(",", array.Select((x) => x.ToString<T>()));
        }
        public static string ToListString<T>(this IList<T> array, string formatting, string separator = ",") where T : IFormattable
        {
            return string.Join(separator, array.Select((x) => x.ToString<T>(formatting, null)));
        }
        public static string ToListString<T>(this T[,] array, string formatting, string itemSeparator = ",", string rowSeparator = "|") where T : IFormattable
        {
            string[] items = new string[array.GetLength(0)];
            for (int i = 0; i<items.Length; i++)
            {
                var row = new T[array.GetLength(1)];
                for (int j = 0; j<row.Length; j++)
                {
                    row[j]=array[i, j];
                }
                items[i]=row.ToListString(formatting, itemSeparator);
            }
            return $"{rowSeparator}{string.Join(rowSeparator, items)}{rowSeparator}";
        }
        static string AlignString(string item, int width, HorizontalAlignment alignment)
        {
            return alignment switch
            {
                HorizontalAlignment.Left => item.PadRight(width),
                HorizontalAlignment.Right => item.PadLeft(width),
                HorizontalAlignment.Center => item.PadLeft(width/2).PadRight(width),
                _ => throw new NotSupportedException($"{alignment.ToString()}"),
            };
        }
        public static string ToVectorTableString<T>(this IList<T> data, HorizontalAlignment alignment, string formatting = null, IFormatProvider provider = null, char leftDelimiter = '|', char rightDelimiter = '|')
        {
            provider??=CultureInfo.CurrentCulture.NumberFormat;
            formatting??=DefaultFormatting;
            string[] items = new string[data.Count];
            for (int i = 0; i<items.Length; i++)
            {
                items[i]=data[i].ToString<T>(formatting, provider);
            }
            int width = items.Max( itm=> itm.Length );
            for (int i = 0; i<items.Length; i++)
            {
                items[i]=AlignString(items[i], width, alignment);
            }
            var sb = new StringBuilder(2*width*items.Length);
            for (int i = 0; i<items.Length; i++)
            {
                sb.Append($"{leftDelimiter} ");
                sb.Append(items[i]);
                sb.Append($" {rightDelimiter}");
                sb.AppendLine();
            }
            return sb.ToString();
        }
        public static string ToTableString<T>(this T[,] data, HorizontalAlignment alignment, string formatting = null, IFormatProvider provider = null, char leftDelimiter = '|', char rightDelimiter = '|')
        {
            provider??=CultureInfo.CurrentCulture.NumberFormat;
            formatting??=DefaultFormatting;
            int n = data.GetLength(0);
            int m = data.GetLength(1);
            string[,] items = new string[n, m];
            for (int i = 0; i<n; i++)
            {
                for (int j = 0; j<m; j++)
                {
                    items[i, j]=data[i, j].ToString<T>(formatting, provider);
                }
            }
            int[] widths = new int[m];
            for (int j = 0; j<m; j++)
            {
                int w = 0;
                for (int i = 0; i<n; i++)
                {
                    w=int.Max(w, items[i, j].Length);
                }
                widths[j]=w;
            }
            for (int i = 0; i<n; i++)
            {
                for (int j = 0; j<m; j++)
                {
                    items[i, j]=AlignString(items[i, j], widths[j], alignment);
                }
            }
            var sb = new StringBuilder(2*widths.Sum()*items.Length);
            for (int i = 0; i<n; i++)
            {
                sb.Append($"{leftDelimiter} ");
                for (int j = 0; j<m; j++)
                {
                    if (j>0) sb.Append($" ");
                    sb.Append(items[i, j]);
                }
                sb.Append($" {rightDelimiter}");
                sb.AppendLine();
            }
            return sb.ToString();
        }
        public static string ToBlockTableString<T>(this IList<T>[] data, HorizontalAlignment alignment, string formatting = null, IFormatProvider provider = null, char leftDelimiter = '|', char rightDelimiter = '|')
        {
            provider??=CultureInfo.CurrentCulture.NumberFormat;
            formatting??=DefaultFormatting;
            var sb = new StringBuilder();
            int width = 0;
            string[][] items = new string[data.Length][];
            for (int k = 0; k<data.Length; k++)
            {
                var block = new string[data[k].Count];
                for (int i = 0; i<block.Length; i++)
                {
                    block[i]=data[k][i].ToString<T>(formatting, provider);
                    width=int.Max(width, block[i].Length);
                }
                items[k]=block;
            }
            for (int k = 0; k<items.Length; k++)
            {
                var block = items[k];
                for (int i = 0; i<block.Length; i++)
                {
                    block[i]=AlignString(block[i], width, alignment);
                }
                for (int i = 0; i<block.Length; i++)
                {
                    sb.Append($"{leftDelimiter} ");
                    sb.Append(block[i]);
                    sb.Append($" {rightDelimiter}");
                    sb.AppendLine();
                }
                if (k!=items.Length-1)
                {
                    sb.Append($"{leftDelimiter} ");
                    sb.Append(new string('-', width));
                    sb.Append($" {rightDelimiter}");
                    sb.AppendLine();
                }
            }
            return sb.ToString();
        }
        public static string ToFixedColumnString<T>(this T[] data, int width, string formatting = null, IFormatProvider provider = null, char leftDelimiter = '|', char rightDelimiter = '|')
        {
            provider??=CultureInfo.CurrentCulture.NumberFormat;
            formatting??=DefaultFormatting;
            StringBuilder sb = new StringBuilder();
            int n = data.Length;
            for (int i = 0; i<n; i++)
            {
                sb.Append(leftDelimiter);
                string text;
                var obj = data[i];
                if (obj is double x)
                {
                    text=Math.Round(x).ToString(formatting, provider);
                }
                else
                {
                    text=obj.ToString(formatting, provider);
                }
                text=text.PadLeft(width);
                if (text.Length>width)
                {
#pragma warning disable IDE0057 // Use range operator
                    text=$"({text.Substring(0, width-3)}…)";  // …
#pragma warning restore IDE0057 // Use range operator
                }
                sb.Append($" {text} ");
                sb.Append(rightDelimiter);
                sb.AppendLine();
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
            for (int i = 0; i<n; i++)
            {
                sb.Append('|');
                for (int j = 0; j<m; j++)
                {
                    var obj = data[j, i];
                    string text = obj.ToString(formatting, provider);
                    text=text.PadLeft(width);
                    if (text.Length>width)
                    {
#pragma warning disable IDE0057 // Use range operator
                        text=$"({text.Substring(0, width-3)}…)";  // …
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
            (x_min, x_max)=(Math.Min(x_min, x_max), Math.Max(x_min, x_max));
            double span = x_max  - x_min;
            int exp = (int)Math.Floor(Math.Log10(span));
            double div = Math.Pow(10, exp);
            double f = x/div;
            // x = f * 10^exp
            f=Math.Round(divisions*f)/divisions;
            return f*div;
        }

    }
}
