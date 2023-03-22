package CmdExecutor;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class CmdExecutor {
    public static String execute(String cmd, String... args) throws IOException, InterruptedException {
        return execute(false, cmd, args);
    }

    public static String execute(boolean allowNonNilExitCode, String cmd, String... args) throws IOException, InterruptedException {
        var command = new String[args.length + 1];
        command[0] = cmd;
        for (int i = 0; i < args.length; i++) {
            command[i+1] = args[i];
        }

        var process = Runtime.getRuntime()
            .exec(command);
        
        var stream = process.getInputStream();
        var sb = new StringBuilder();
        new BufferedReader(new InputStreamReader(stream)).lines().forEach(line -> {
            sb.append(line);
            sb.append("\n");
        });
        if (sb.length() > 0 && sb.charAt(sb.length() - 1) == '\n') {
            sb.deleteCharAt(sb.length() - 1);
        }

        var exitCode = process.waitFor();
        if (exitCode != 0) {
            var err = process.getErrorStream();
            var newSb = new StringBuilder();
            new BufferedReader(new InputStreamReader(err)).lines().forEach(line -> {
                newSb.append(line);
                newSb.append("\n");
            });
            if (newSb.length() > 0 && newSb.charAt(newSb.length() - 1) == '\n') {
                newSb.deleteCharAt(newSb.length() - 1);
            }
            
            if (allowNonNilExitCode) {
                return String.valueOf(exitCode);
            }
            System.out.println(newSb.toString());
            System.out.println("Exited with non-zero code!");
            System.exit(exitCode);
        }

        return sb.toString();
    }
}
