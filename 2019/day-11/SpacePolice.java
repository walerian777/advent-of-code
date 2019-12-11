// https://adventofcode.com/2019/day/11

import java.io.File;
import java.io.FileNotFoundException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Scanner;

public class SpacePolice {
  static class IntcodeComputer {
    private int[] memory;
    private int instructionPointer = 0;
    private int relativeBase = 0;
    private int output = 0;
    private boolean done = false;
    Queue<Integer> inputQueue = new LinkedList<>();

    IntcodeComputer(ArrayList<Integer> input) {
      int[] initialMemory = input.stream().mapToInt(i -> i).toArray();
      memory = Arrays.copyOf(initialMemory, initialMemory.length + 1000);
      Arrays.fill(memory, initialMemory.length, initialMemory.length + 1000, 0);
    }

    public int execute(int input) {
      inputQueue.add(input);
      return execute();
    }

    public int execute() {
      while (instructionPointer < memory.length) {
        int instruction = memory[instructionPointer];
        int opcode = instruction % 100;
        int firstParamMode = instruction / 100 % 10;
        int secondParamMode = instruction / 1000 % 10;
        int resultParamMode = instruction / 10000 % 10;

        int firstParam = fetchParam(firstParamMode, 1);
        int secondParam = fetchParam(secondParamMode, 2);
        int resultParam = fetchParam(resultParamMode, 3);

        switch (opcode) {
          case 1:
            memory[resultParam] = memory[firstParam] + memory[secondParam];
            instructionPointer += 4;
            break;
          case 2:
            memory[resultParam] = memory[firstParam] * memory[secondParam];
            instructionPointer += 4;
            break;
          case 3:
            memory[firstParam] = inputQueue.remove();
            instructionPointer += 2;
            break;
          case 4:
            output = memory[firstParam];
            instructionPointer += 2;
            return output;
          case 5:
            if (memory[firstParam] != 0) {
              instructionPointer = memory[secondParam];
            } else {
              instructionPointer += 3;
            }
            break;
          case 6:
            if (memory[firstParam] == 0) {
              instructionPointer = memory[secondParam];
            } else {
              instructionPointer += 3;
            }
            break;
          case 7:
            memory[resultParam] = memory[firstParam] < memory[secondParam] ? 1 : 0;
            instructionPointer += 4;
            break;
          case 8:
            memory[resultParam] = memory[firstParam] == memory[secondParam] ? 1 : 0;
            instructionPointer += 4;
            break;
          case 9:
            relativeBase += memory[firstParam];
            instructionPointer += 2;
            break;
          case 99:
            done = true;
            return output;
          default:
            throw new RuntimeException("Oops");
        }
      }

      return 0;
    }

    public boolean isDone() {
      return done;
    }

    private int fetchParam(int mode, int offset) {
      int pointer = instructionPointer + offset;
      int param = 0;

      switch (mode) {
        case 0:
          param = memory[pointer];
          break;
        case 1:
          param = pointer;
          break;
        case 2:
          param = relativeBase + memory[pointer];
          break;
      }

      return param;
    }
  }

	static class Coordinate {
		int x;
		int y;

		public boolean equals(Object other) {
			Coordinate coordinate = (Coordinate) other;
			return coordinate.x == x && coordinate.y == y;
		}

		public Coordinate(int x, int y) {
			this.x = x;
			this.y = y;
		}

		public int hashCode() {
      return x * 10 + y;
		}

    public String toString() {
      return x + ", " + y;
    }
	}

  static class Panel {
    public Coordinate coordinate;
    public boolean painted = false;
    public int color = 0;

    public Panel(int x, int y) {
      this.coordinate = new Coordinate(x, y);
    }

    public Panel(Coordinate coordinate, int color) {
      this.coordinate = coordinate;
      this.color = color;
    }

    public Panel(int x, int y, int color) {
      this.coordinate = new Coordinate(x, y);
      this.color = color;
    }
  }

  public static void main(String[] args) {
    ArrayList<Integer> input = loadInput();

    IntcodeComputer computer = new IntcodeComputer(input);
    Panel currentPanel = new Panel(0, 0, 0);
    Integer currentDirection = 0;
    ArrayList<Panel> visitedPanels = new ArrayList<Panel>();

    while(!computer.isDone()) {
      int color = computer.execute(currentPanel.color);
      visitedPanels.add(new Panel(currentPanel.coordinate.x, currentPanel.coordinate.y, color));
      currentDirection = computer.execute() == 1 ? currentDirection + 1 : currentDirection - 1;
      currentDirection = currentDirection % 4;
      currentPanel = new Panel(computePosition(currentDirection, currentPanel.coordinate), 0);
    }

    System.out.println(visitedPanels.size());
  }

  private static ArrayList<Integer> loadInput() {
    File file = new File("input");
    Scanner scanner = null;

    try {
      scanner = new Scanner(file);
    } catch(Exception _e) {
      System.out.println("Couldn't read file");
    }

    ArrayList<Integer> input = new ArrayList<Integer>();
    scanner.useDelimiter(",");

    while(scanner.hasNextInt()) {
      input.add(scanner.nextInt());
    }
    scanner.close();

    return input;
  }

  private static Coordinate computePosition(int direction, Coordinate coordinate) {
    int x = coordinate.x;
    int y = coordinate.y;

    switch (direction) {
      case 0:
        y--;
        break;
      case 1:
        x++;
        break;
      case 2:
        y++;
        break;
      case 3:
        x--;
        break;
    }

    return new Coordinate(x, y);
  }
}
