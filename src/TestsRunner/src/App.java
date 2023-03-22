import ArgPar.Annotation.ParsableArgument;
import ArgPar.Annotation.ParsableCommand;
import ArgPar.Annotation.ParsableFlag;
import ArgPar.Exception.ParseException;
import ArgPar.Parser.ArgumentParser;

import static CmdExecutor.CmdExecutor.*;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;

public class App {
    @ParsableCommand(commandName = "test", description = "PINS Compiler test tools")
    public static class CLI {
        @ParsableArgument
        public String pathToCompiler;

        @ParsableArgument
        public String pathToTests;

        @ParsableFlag(name = {"--verbose", "-v"})
        public boolean verbose;

        @ParsableFlag(name = "-s")
        public boolean stopAtFirst;

        private final String testsDir = ".test/";

        public static void main(String[] args) throws Exception {
            var parser = new ArgumentParser<>(CLI.class);
            var cli = parser.parse(args);
            cli.runTests();
        }

        private void runTests() throws IOException, InterruptedException {
            removeDir(testsDir);
            mkdir(testsDir);
            compileCompiler();
            if (pathToTests.trim().endsWith("*")) {
                var dir = new File(pathToTests.trim().substring(0, pathToTests.length() - 2));
                if (!dir.isDirectory()) { 
                    System.err.println("Napačna pot do direktorija!");
                    System.exit(123);   
                }
                var allFiles = dir.listFiles();
                Arrays.sort(allFiles);
                
                var results = new ArrayList<Pair<TestsFileParser.TestFile, Integer>>(allFiles.length);
                var longest = "";
                for (var file : allFiles) {
                    results.add(runTests(file.getPath()));
                    if (longest.length() < file.getPath().length()) {
                        longest = file.getPath();
                    }
                }
                var i = 0;
                var allTests = 0;
                var succeeded = 0;

                System.out.println();
                System.out.println("Final report:");
                System.out.println("-------------------------");
                for (var result : results) {
                    var indent = longest.length() - allFiles[i].getPath().length();
                    var sb = new StringBuilder();
                    for (int j = 0; j < indent; j++) {
                        sb.append(" ");
                    }
                    var split = allFiles[i].getPath().split("/");
                    var test = split[split.length - 1];
                    System.out.println(test + sb.toString() + ": " + result.snd + " / " + result.fst.tests.size());
                    i++;
                    allTests += result.fst.tests.size();
                    succeeded += result.snd;
                }

                var percentFormatter = NumberFormat.getPercentInstance();
                var perc = allTests == 0 ? 1 : (double) succeeded / (double) allTests;

                System.out.println("Result: " + succeeded + " / " + allTests);
                System.out.println("   Score: " + percentFormatter.format(perc));
                System.out.println("   Grade: " + gradeForPerc(perc));
            } else {
                runTests(pathToTests);
            }
            removeDir(testsDir);
        }

        private Pair<TestsFileParser.TestFile, Integer> runTests(String pathToTestFile) throws IOException, InterruptedException {
            var parser = new TestsFileParser();
            var testsFile = parser.parseFile(pathToTestFile);
            var tests = testsFile.tests;

            System.out.println("[" + pathToTestFile + "] Running " + tests.size() + " tests:");

            var succeeded = 0;
            for (var test : tests) {
                var file = testsDir + test.name + "-test.pins";
                storeCodeToFile(test.code, file);
                var result = runCode(file, testsFile.compilerFlags);

                var isFailure = true;
                var sign = test.isFailureTest ? "👎" : "👍";
                for (var expected : test.expectedOutputs) {
                    if (result.equals(expected)) {
                        System.out.println("✅ test '" + test.name + "' succeeded ["+sign+"]!");
                        succeeded += 1;
                        isFailure = false;
                        break;
                    }
                }
                if (isFailure) {
                    System.out.println("❌ test '" + test.name + "' failed ["+sign+"]!");
                    if (verbose) {
                        var allExpectedOutputs = test.expectedOutputs.stream()
                                .collect(Collectors.joining("<or>\n"));
                        System.out.println("Input:\n" + test.code);
                        System.out.println("Expected: \n" + allExpectedOutputs);
                        System.out.println("Got: \n" + result);
                    }
                    if (stopAtFirst) break;
                }
            }

            if (succeeded == tests.size()) {
                System.out.println("✅ " + succeeded + " out of " + tests.size() + " tests passed!");
            } else {
                System.out.println("❌ " + succeeded + " out of " + tests.size() + " tests passed!");
            }

            var percentFormatter = NumberFormat.getPercentInstance();
            var perc = tests.isEmpty()
                ? 1
                : (double) succeeded / (double) tests.size();
            System.out.println("   Score: " + percentFormatter.format(perc));
            System.out.println("   Grade: " + gradeForPerc(perc));

            return new Pair<TestsFileParser.TestFile, Integer>(testsFile, succeeded);
        }

        private void mkdir(String dir) {
            new File(dir).mkdir();
        }

        private void storeCodeToFile(String code, String file) throws IOException {
            var writer = new BufferedWriter(new FileWriter(file));
            writer.write(code);
            writer.close();
        }

        private void compileCompiler() throws IOException, InterruptedException {
            var files = allJavaFiles();
            compileJavaProject(files);
        }

        private String[] allJavaFiles() throws IOException, InterruptedException {
            var files = execute("find", pathToCompiler + "src/", "-type", "f", "-name", "*.java");
            var lines = files.split("\n");
            return lines;
        }

        private void compileJavaProject(String[] javaFiles) throws IOException, InterruptedException {
            var compileCodeCommand = new String[javaFiles.length + 4];
            compileCodeCommand[javaFiles.length + 0] = "-cp";
            compileCodeCommand[javaFiles.length + 1] = ".:" + pathToCompiler + "lib/*";
            compileCodeCommand[javaFiles.length + 2] = "-d";
            compileCodeCommand[javaFiles.length + 3] = testsDir;
            for (int i = 0; i < javaFiles.length; i++) {
                compileCodeCommand[i] = javaFiles[i];
            }
            execute("javac", compileCodeCommand);
        }

        private String runCode(String pathToInputFile, Optional<String[]> flags) throws IOException, InterruptedException {
            final var libPath = testsDir + ":" + pathToCompiler + "lib/*";
            final var program = "Main";
            String[] args;
            if (flags.isPresent()) {
                var offset = 5;
                args = new String[flags.get().length + offset];
                args[0] = "-cp";
                args[1] = libPath;
                args[2] = program;
                args[3] = "PINS";
                args[4] = pathToInputFile;
                for (int i = 0; i < flags.get().length; i++) {
                    args[i+offset] = flags.get()[i];
                }
            } else {
                args = new String[] {"-cp", libPath, program, "PINS", pathToInputFile, "--test"};
            }
            var result = execute(true, "java", args);
            return result;
        }
    
        private void removeDir(String dir) throws IOException, InterruptedException {
            execute("rm", "-rf", dir);
        }

        private int gradeForPerc(double perc) {
            if (perc < 0.5) {
                return 5;
            }
            if (perc < 0.6) {
                return 6;
            }
            if (perc < 0.7) {
                return 7;
            }
            if (perc < 0.8) {
                return 8;
            }
            if (perc < 0.9) {
                return 9;
            }
            return 10;
        }
    }

    static class Pair<A, B> {
        public final A fst;
        public final B snd;

        public Pair(A fst, B snd) {
            this.fst = fst;
            this.snd = snd;
        }
    }

    public static void main(String[] args) throws Exception {
        try {
            CLI.main(args);
        } catch (ParseException e) {}
    }
}
