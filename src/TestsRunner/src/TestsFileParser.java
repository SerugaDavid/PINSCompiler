import static java.util.Objects.requireNonNull;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class TestsFileParser {
    public TestFile parseFile(String file) throws IOException {
        List<Test> tests = new ArrayList<>();
        var reader = new BufferedReader(new FileReader(file));

        String[] flags = null;

        // @note: compiler flags have to be provided as the first line of the
        // tests file.
        // @todo: make it more flexible in future.
        var line = reader.readLine();
        if (line != null && line.startsWith("!compiler_flags:")) {
            var spl = line.split(":");
            flags = spl[1].trim().split(" ");
            line = null;
        }

        while (true) {
            var test = parseTest(reader, line, tests.size() + 1);
            line = null;
            if (test.isPresent()) {
                tests.add(test.get());
            } else {
                break;
            }
        }

        reader.close();
        return new TestFile(Optional.ofNullable(flags), tests);
    }

    private Optional<Test> parseTest(BufferedReader reader, String fistLine, int counter) throws IOException {
        final var nameBadge     = "!name:";
        final var codeBadge     = "!code:";
        final var expectedBadge = "!expected:";
        final var orBadge       = "!or:";
        final var failureBadge  = "!failure:";
        final var endBadge      = "!end";

        var line = fistLine == null ? reader.readLine() : fistLine;
        while (line != null && (line.startsWith("--") || line.isBlank())) {
            // skip comments and blanks
            line = reader.readLine();
        }
        if (line == null) {
            return Optional.empty();
        }

        final String testName;
        if (line.startsWith(nameBadge)) {
            var spl = line.split(":", 2);
            if (spl.length != 2) {
                throw new IOException("Name badge expects name parameter!");
            }
            testName = spl[1].trim();
            line = reader.readLine();
        } else {
            testName = "program" + counter;
        }
        
        if (line == null) {
            throw new IOException("Unexpected eof!");
        }
        if (!line.startsWith(codeBadge)) {
            throw new IOException("Expected " + codeBadge + "!");
        }

        line = reader.readLine();

        var sb = new StringBuilder();
        while (line != null && !(line.startsWith(expectedBadge) || line.startsWith(failureBadge))) {
            sb.append(line);
            sb.append("\n");
            line = reader.readLine();
        }
        var isFailureTest = line.startsWith(failureBadge);

        if (sb.length() > 0 && sb.charAt(sb.length() - 1) == '\n')
            sb.deleteCharAt(sb.length() - 1);

        final var testCode = sb.toString();

        sb = new StringBuilder();
        line = reader.readLine();

        var testOutputs = new ArrayList<String>();

        while (true) {
            while (line != null && !(line.startsWith(endBadge) || line.startsWith(orBadge) )) {
                sb.append(line);
                sb.append("\n");
                line = reader.readLine();
            }
            if (sb.length() > 0 && sb.charAt(sb.length() - 1) == '\n')
                sb.deleteCharAt(sb.length() - 1);
            
            testOutputs.add(sb.toString());

            if (line != null && line.startsWith(orBadge)) {
                sb = new StringBuilder();
                line = reader.readLine();
            } else {
                break;
            }
        }
        
        return Optional.of(new Test(testName, testCode, testOutputs, isFailureTest));
    }

    public static class TestFile {
        public final Optional<String[]> compilerFlags;
        public final List<Test> tests;

        public TestFile(Optional<String[]> compilerFlags, List<Test> tests) {
            requireNonNull(compilerFlags);
            requireNonNull(tests);
            this.compilerFlags = compilerFlags;
            this.tests = tests;
        }
    }

    public static class Test {
        public final String name;
        public final String code;
        public final List<String> expectedOutputs;
        public final boolean isFailureTest;

        public Test(String name, String code, List<String> expectedOutputs, boolean isFailureTest) {
            requireNonNull(name);
            requireNonNull(code);
            requireNonNull(expectedOutputs);
            this.name = name;
            this.code = code;
            this.expectedOutputs = expectedOutputs;
            this.isFailureTest = isFailureTest;
        }

        @Override
        public String toString() {
            var sb = new StringBuilder();
            sb.append("Name: ");
            sb.append(name);
            sb.append("\nCode:\n");
            sb.append(code);
            sb.append("\nOutput:\n");
            sb.append(expectedOutputs.stream().collect(Collectors.joining("\n")));
            return sb.toString();
        }
    }
}
