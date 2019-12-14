using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SpaceStoichiometry
{
    class ChemicalReaction
    {
        public ChemicalReaction(Dictionary<string, int> inputChemicals, Tuple<string, int> outputChemical)
        {
            InputChemicals = inputChemicals;
            OutputChemical = outputChemical;
        }

        public Dictionary<string, int> InputChemicals { get; }
        public Tuple<string, int> OutputChemical { get; }
    }
    class Program
    {
        static void Main(string[] args)
        {
            var reactions = FetchChemicalReactions();
            var oreNeeded = calculateOreNeededToProduceFuel(1, reactions);
            
            Console.WriteLine(oreNeeded);
            Console.ReadKey();
        }

        static Dictionary<string, ChemicalReaction> FetchChemicalReactions()
        {
            var reactions = new Dictionary<string, ChemicalReaction>();
            var input = System.IO.File.ReadAllLines("input")
                .Select(line => line.Split(new[] { " => " }, 0));

            foreach (var line in input)
            {
                var leftHandSide = line[0].Split(new[] { ", " }, 0);
                var righHandSide = line[1].Split(' ');
                var inputChemicals = leftHandSide
                    .Select(chemical => chemical.Split(' '))
                    .ToDictionary(chemical => chemical[1], chemical => int.Parse(chemical[0]));
                var outputChemical = new Tuple<string, int>(righHandSide[1], int.Parse(righHandSide[0]));
                reactions.Add(outputChemical.Item1, new ChemicalReaction(inputChemicals, outputChemical));
            }

            return reactions;
        }

        static int calculateOreNeededToProduceFuel(int fuelAmount, Dictionary<string, ChemicalReaction> availableReactions)
        {
            var needs = new Dictionary<string, int> { { "FUEL", fuelAmount } };

            while (needs.Any(need => need.Key != "ORE" && need.Value > 0))
            {
                var chemical = needs.First(need => need.Key != "ORE" && need.Value > 0);
                var reaction = availableReactions[chemical.Key];
                needs[chemical.Key] -= reaction.OutputChemical.Item2;
                foreach (var inputChemical in reaction.InputChemicals)
                {
                    if (needs.ContainsKey(inputChemical.Key))
                    {
                        needs[inputChemical.Key] += inputChemical.Value;
                    }
                    else
                    {
                        needs.Add(inputChemical.Key, inputChemical.Value);
                    }
                }
            }

            return needs["ORE"];
        }
    }
}
