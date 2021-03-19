using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace L_system
{
    class Lsystem
    {
        public string Axiom { get; }
        public string Sentence { get; private set; }
        public Dictionary<char, string> Rules { get; set; }
        public int Generation { get; private set; }
        public Lsystem(string axiom, Dictionary<char, string> rules = null)
        {
            Axiom = axiom;
            Sentence = axiom;
            Generation = 0;
            Rules = rules ?? new Dictionary<char, string>();
        }

        public void RemoveAllLetterRules(char letter)
        {
            Rules.Remove(letter);
        }


        /// <summary>
        /// Removes all rules from rules dictionary by using value (string rewriteRule)
        /// </summary>
        /// <param name="rewriteRule"></param>
        public void RemoveAllRewriteRules(string rewriteRule)
        {
            var pairEnumerable = Rules.Where(x => x.Value == rewriteRule);
            foreach (var pair in pairEnumerable)
            {
                if (default(KeyValuePair<char, string>).Key != pair.Key)
                {
                    Rules.Remove(pair.Key);
                }
            }
        }

        /// <summary>
        /// Generates the next generation, the user is responsible for properly defined rules
        /// </summary>
        public void NextGeneration()
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < Sentence.Length; ++i)
            {
                if (!IsNotMovingChar(Sentence[i]))
                {
                    if (Rules.ContainsKey(Sentence[i]))
                    {
                        sb.Append(Rules[Sentence[i]]);
                    }
                    else
                    {
                        throw new KeyNotFoundException($"Missing rule for {Sentence[i]}");
                    }
                }
                else
                {
                    sb.Append(Sentence[i]);
                }
            }
            Sentence = sb.ToString();
            ++Generation;
        }

        public static bool IsNotMovingChar(char c)
        {
            return c == '+' || c == '-' || c == '[' || c == ']';
        }

        /// <summary>
        /// Calculates the desired generation does nothing when Generation == generation
        /// </summary>
        /// <param name="generation"></param>
        public void NthGeneration(int generation)
        {
            if (generation > Generation)
            {
                for (int i = Generation; i < generation; ++i)
                {
                    NextGeneration();
                }

            }
            else if (generation < Generation)
            {
                Generation = 0;
                Sentence = Axiom;
                NthGeneration(generation);
            }
        }
    }

}
