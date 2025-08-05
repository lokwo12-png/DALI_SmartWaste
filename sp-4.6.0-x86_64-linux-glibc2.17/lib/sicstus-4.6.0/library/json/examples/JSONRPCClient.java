import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.ProcessBuilder.Redirect;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import jdk.nashorn.api.scripting.ScriptObjectMirror;
/*
 * Copyright (C) 2020, RISE Research Institutes of Sweden AB.
 */

/**
 * A simple example of invoking SICStus as a sub-process, and passing data back
 * and forth using JSON-RPC messages. The example includes backtracking over
 * solutions.
 *
 * The example code contains some rudimentary JSON and JSON-RPC functionality
 * but it is not intended for production use. There are many full-featured JSON
 * and JSON-RPC implementations available elsewhere.
 *
 * For questions, contact <tt>sicstus-support@sics.se</tt>.
 *
 */

/*-
 * Example transcript (macOS/Linux):
 * bash$ pwd
 * /tmp/json_test
 * bash$ ls
 * bash$ javac -d . /usr/local/sicstus4.5.0/lib/sicstus-4.5.0/library/json/examples/JSONRPCClient.java && jar cf JSONRPCClient.jar JSONRPCClient*.class && java -Dlogging-false -cp JSONRPCClient.jar JSONRPCClient /usr/local/sicstus4.5.0/bin/sicstus
 * state ==> State is: JSONObject [NULL: iJSONObject=null]
 * state:=4 ==> State was: JSONObject [NULL: iJSONObject=null]
 * state ==> State is: JSONObject [NUMBER: iJSONObject=4]
 *
 * once Result is StateIn+1, StateOut=Result. ==> Result=JSONObject [NUMBER: iJSONObject=5]
 * once Result is StateIn+1, StateOut=Result. ==> Result=JSONObject [NUMBER: iJSONObject=6]
 *
 * once Result is StateIn+Increment,StateOut=Result. ==> Result=JSONObject [NUMBER: iJSONObject=11]
 *
 * call Multiplier=10, member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc. ==> First Result=JSONObject [NUMBER: iJSONObject=111]
 * retry ==> (next) Result=JSONObject [NUMBER: iJSONObject=211]
 * cut ==> Result=JSONObject [NULL: iJSONObject=null]
 *
 * call Multipleir=10,member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc. ==> First Result=JSONObject [NUMBER: iJSONObject=111]
 * retry ==> (next) Result=JSONObject [NUMBER: iJSONObject=211]
 * retry ==> (next) Result=JSONObject [NUMBER: iJSONObject=311]
 * retry ==> (next) Result failed (this is expected)
 *
 * once foo is bar. ==> Result threw exception (this is expected) JSONObject [STRING: iJSONObject=type_error(evaluable,bar/0)]
 *
 * quit ==> Result=JSONObject [STRING: iJSONObject=Bye]
 * bash$
 *
 * (In Java 11 you may get warnings that Nashorn JavaScript engine has been deprecated.)
 */
@SuppressWarnings({"boxing","removal"})
public class JSONRPCClient {
    private static final int PROLOG_ERROR_CODE_FAIL = -4711;
    private static final int PROLOG_ERROR_CODE_EXCEPTION = -4712;

    private static final boolean RUN_TESTS = Boolean.getBoolean("runTests");
    private static final boolean LOGGING = Boolean.getBoolean("logging");

    /**
     * Simple adapter from the JavaScript objects created by Nashorn.
     *
     * The <tt>asKIND</tt> methods, e.g. {@link #asObject()}, returns null (or a
     * default value) if the underlying JavaScript object is not of the desired
     * {@link #getKind() KIND}.
     */
    public static class JSONValue {

        private static final JSONValue[] EMPTY_JSON_VALUE_ARRAY = new JSONValue[0];
        private static final Object EMPTY_OBJECT_MARKER = new char[0];
        private static final Object EMPTY_ARRAY_MARKER = new char[0];

        public enum Kind {
            OTHER, ARRAY, OBJECT, STRING, NUMBER, BOOLEAN, NULL
        }

        private final Object iJSONObject;

        protected JSONValue(Object jsonObject) {
            iJSONObject = jsonObject;
        }

        public static JSONValue create(Object jsonObject) {
            return new JSONValue(jsonObject);
        }

        public static JSONValue emptyObject() {
            return JSONValue.create(EMPTY_OBJECT_MARKER);
        }

        public static JSONValue emptyArray() {
            return JSONValue.create(EMPTY_ARRAY_MARKER);
        }

        public Kind getKind() {
            if (iJSONObject instanceof ScriptObjectMirror) {
                if (((ScriptObjectMirror) iJSONObject).isArray()) {
                    return Kind.ARRAY;
                }
                return Kind.OBJECT;
            }
            if (iJSONObject instanceof String) {
                return Kind.STRING;
            }
            if (iJSONObject instanceof Boolean) {
                return Kind.BOOLEAN;
            }
            if (iJSONObject instanceof Number) {
                return Kind.NUMBER;
            }
            if (iJSONObject == null) {
                return Kind.NULL;
            }
            if (iJSONObject == EMPTY_OBJECT_MARKER) {
                return Kind.OBJECT;
            }
            if (iJSONObject == EMPTY_ARRAY_MARKER) {
                return Kind.ARRAY;
            }
            return Kind.OTHER;
        }

        public JSONValue[] asArray(JSONValue[] defaultValue) {
            if (iJSONObject instanceof ScriptObjectMirror) {
                ScriptObjectMirror scriptObjectMirror = (ScriptObjectMirror) iJSONObject;
                if (scriptObjectMirror.isArray()) {
                    int size = scriptObjectMirror.size();
                    JSONValue[] ar = new JSONValue[size];

                    for (int i = 0; i < size; i++) {
                        ar[i] = JSONValue.create(scriptObjectMirror.getSlot(i));
                    }
                    return ar;
                }
            }
            if (iJSONObject == EMPTY_ARRAY_MARKER) {
                return EMPTY_JSON_VALUE_ARRAY;
            }

            return defaultValue;
        }

        public JSONValue[] asArray() {
            return asArray(null);
        }

        public Map<String, JSONValue> asObject(Map<String, JSONValue> defaultValue) {
            if (iJSONObject instanceof ScriptObjectMirror) {
                ScriptObjectMirror scriptObjectMirror = (ScriptObjectMirror) iJSONObject;
                if (!scriptObjectMirror.isArray()) {
                    int size = scriptObjectMirror.size();
                    Map<String, JSONValue> map = new LinkedHashMap<>(size * 2);

                    for (Entry<String, Object> e : scriptObjectMirror.entrySet()) {
                        map.put(e.getKey(), JSONValue.create(e.getValue()));
                    }
                    return map;
                }
            }
            if (iJSONObject == EMPTY_OBJECT_MARKER) {
                return Collections.emptyMap();
            }

            return defaultValue;
        }

        public Map<String, JSONValue> asObject() {
            return asObject(null);
        }

        public String asString(String defaultValue) {
            if (iJSONObject instanceof String) {
                return (String) iJSONObject;
            }
            return defaultValue;
        }

        public String asString() {
            return asString(null);
        }

        public Number asNumber(Number defaultValue) {
            if (iJSONObject instanceof Number) {
                return (Number) iJSONObject;
            }
            return defaultValue;
        }

        public Number asNumber() {
            return asNumber(null);
        }

        public Boolean asBoolean(Boolean defaultValue) {
            if (iJSONObject instanceof Boolean) {
                return (Boolean) iJSONObject;
            }
            return null;
        }

        public Boolean asBoolean() {
            return asBoolean(null);
        }

        public boolean isNull() {
            return iJSONObject == null;
        }

        @Override
        public String toString() {
            return "JSONObject [" + getKind() + ": iJSONObject=" + iJSONObject + "]";
        }
    }

    public static class JSONParser {
        private final ScriptEngine iEngine;

        public static JSONParser create() {
            return new JSONParser();
        }

        protected JSONParser() {
            ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
            if (engine == null) {
                throw new Error("Could not instantiate Nashorn JavaScript engine.");
            }
            iEngine = engine;
        }

        public static class JSONParseError extends RuntimeException {
            private static final long serialVersionUID = 1L;

            public JSONParseError(String message, Throwable t) {
                super(message, t);
            }
        }

        public JSONValue parse(String json) {
            Object o;
            try {
                o = iEngine.eval(String.format("JSON.parse('%s')", json));
            } catch (ScriptException e) {
                throw new JSONParseError("Error parsing JSON \"" + json + "\"", e);
            }

            return JSONValue.create(o);
        }
    }

    public static class JSONWriter {
        public static class JSONWriterError extends RuntimeException {
            private static final long serialVersionUID = 1L;

            public JSONWriterError(String message) {
                super(message);
            }
        }

        public static class JSONWrapper {
            private final Object iObject;

            protected JSONWrapper(Object o) {
                iObject = o;
            }

            public static JSONWrapper create(Object o) {
                return new JSONWrapper(o);
            }

            private String toJSON() {
                return toJSON(new StringBuilder()).toString();
            }

            public StringBuilder toJSON(StringBuilder sb) {
                Object o = iObject;
                if (o instanceof Map<?, ?>) {
                    Map<?, ?> map = (Map<?, ?>) o;
                    sb.append('{');
                    String prefix = "";
                    for (Entry<?, ?> e : map.entrySet()) {
                        Object key = e.getKey();
                        if (key instanceof String) {
                            sb.append(prefix);
                            stringToJSON(sb, (String) key);
                            sb.append(": ");
                            create(e.getValue()).toJSON(sb);
                            prefix = ", ";
                        }
                    }
                    sb.append('}');
                } else if (o instanceof List<?>) {
                    List<?> list = (List<?>) o;
                    sb.append('[');
                    String prefix = "";
                    for (Object e : list) {
                        sb.append(prefix);
                        create(e).toJSON(sb);
                        prefix = ", ";
                    }
                    sb.append(']');
                } else if (o instanceof String) {
                    stringToJSON(sb, (String) o);

                } else if (o instanceof Number) {
                    sb.append(o);
                } else if (o instanceof Boolean) {
                    sb.append(((Boolean) o).booleanValue() ? "true" : "false");
                } else if (o == null) {
                    sb.append("null");
                } else {
                    throw new JSONWriterError("Unhandled type: " + o);
                }
                return sb;
            }

            private void stringToJSON(StringBuilder sb, String s) {
                sb.append('"');
                escapeString(sb, s);
                sb.append('"');
            }

            /**
             * Escape characters, as needed, to make them suitable for a JSON string
             *
             * @param sb
             * @param s
             * @return
             */
            private StringBuilder escapeString(StringBuilder sb, String s) {
                for (int i = 0, n = s.length(); i < n; i++) {
                    char c = s.charAt(i);
                    if (c <= 0x1F) {
                        escapeChar(sb, c);
                    } else if (c <= 127) {
                        // ASCII, always unescaped
                        sb.append(c);
                    } else {
                        // Escape everything else (this is not necessary, it should be an option)
                        escapeChar(sb, c);
                    }

                }
                return sb;
            }

            private StringBuilder escapeChar(StringBuilder sb, char c) {
                sb.append("\\u");
                if (c <= 0x0FFF) {
                    sb.append('0');
                }
                if (c <= 0x00FF) {
                    sb.append('0');
                }
                if (c <= 0x000F) {
                    sb.append('0');
                }

                sb.append(Integer.toHexString(c));
                return sb;
            }

            @Override
            public String toString() {
                return "JSONWrapper [iObject=" + iObject + ", JSON=\"" + toJSON() + "\"]";
            }

        }

        public static JSONWriter create() {
            return new JSONWriter();
        }

        protected JSONWriter() {

        }

        public String toJSON(Object o) {
            return JSONWrapper.create(o).toJSON();
        }

    }

    // Returns 0 on success, 1 on failure
    private static int testJSONParser(String jsonText, boolean expectError) {
        JSONParser parser = JSONParser.create();
        JSONValue value;
        try {
            value = parser.parse(jsonText);
        } catch (JSONParser.JSONParseError e) {
            if (expectError) {
                return 0;
            }
            System.err.println("TEST FAILED: error from parser");
            e.printStackTrace();

            return 1;
        }

        JSONValue[] a = value.asArray();
        Map<String, JSONValue> o = value.asObject();
        String s = value.asString();
        Number n = value.asNumber();
        Boolean b = value.asBoolean();
        boolean isNull = value.isNull();

        System.err.println(value + " " + "asArray=" + a + ", asObject=" + o + ", asString=" + s + ", asNumber=" + n
                + ", asBoolean=" + b + ", isNull=" + isNull);

        if (expectError) {
            System.err.println("TEST FAILED: Expected error from parser");
            return 1;
        }
        return 0;

    }

    private static int testJSONParser(String jsonText) {
        return testJSONParser(jsonText, false);
    }

    private static int testJSONParser() {
        int failures = 0;
        failures += testJSONParser("{\"foo\":1, \"bar\":\"baz\"}");
        failures += testJSONParser("{\"foo\":1, \"inner\":{\\\"foo\\\":1, \\\"bar\\\":\\\"baz\\\"}}");
        failures += testJSONParser("[\"foo\",1, \"inner\",{\\\"foo\\\":1, \\\"bar\\\":\\\"baz\\\"}]");
        failures += testJSONParser("\"foo\"");
        failures += testJSONParser("42");
        failures += testJSONParser("42.54");
        failures += testJSONParser("true");
        failures += testJSONParser("false");
        failures += testJSONParser("null");
        // Malformed:
        failures += testJSONParser("{\"foo\":1, 444:{\\\"foo\\\":1, \\\"bar\\\":\\\"baz\\\"}}", true);
        return failures;
    }

    private static int testJSONWriter(Object o, boolean expectError) {
        JSONWriter writer = JSONWriter.create();

        String json;
        try {
            json = writer.toJSON(o);
        } catch (JSONWriter.JSONWriterError e) {
            if (expectError) {
                return 0;
            }
            System.err.println("TEST FAILED: error from writer");
            e.printStackTrace();
            return 1;
        }

        System.err.println(o + "==JSON==> " + json);
        if (expectError) {
            System.err.println("TEST FAILED: Expected error from writer");
            return 1;
        }

        return testJSONParser(json);
    }

    private static int testJSONWriter(Object o) {
        return testJSONWriter(o, false);
    }

    private static int testJSONWriter() {
        int failures = 0;
        failures += testJSONWriter(Integer.valueOf(42));
        failures += testJSONWriter(Double.valueOf(42.34));
        failures += testJSONWriter(null);
        failures += testJSONWriter(Boolean.TRUE);
        failures += testJSONWriter(Boolean.FALSE);
        failures += testJSONWriter("foo");
        failures += testJSONWriter(Arrays.asList("a"));

        Map<String, Object> map = new LinkedHashMap<>();
        Map<String, Object> map2 = new LinkedHashMap<>();

        failures += testJSONWriter(map);

        map.clear();
        map.put("foo", Integer.valueOf(45));
        failures += testJSONWriter(map);

        map2.clear();
        map2.put("bork", "blah");
        map2.put("inner", map);
        map2.put("a list", Arrays.asList("a", map, Boolean.TRUE));
        failures += testJSONWriter(map2);

        // Unhandled value type
        failures += testJSONWriter(new Object(), true);

        return failures;
    }

    static String toRPC(int id, String method, Object params) {
        Map<String, Object> request = new LinkedHashMap<>();

        request.put("jsonrpc", "2.0");
        if (id >= 0) {
            request.put("id", Integer.valueOf(id));
        }
        request.put("method", method);
        if (params != null) {
            request.put("params", params);
        }
        return JSONWriter.create().toJSON(request);
    }

    private static Process launch(String path, String code_path) {
        List<String> command = new ArrayList<>();
        command.add(path);
        if (!LOGGING) {
            command.add("--nologo");
            command.add("--noinfo");
        }
        command.add("-l");
        command.add(code_path);

        command.add("--goal");
        command.add("jsonrpc_server_main([call_hook(call)]),halt.");

        ProcessBuilder pb = new ProcessBuilder(command);
        pb.redirectError(Redirect.INHERIT);

        Process process;
        try {
            process = pb.start();
        } catch (IOException e) {
            if (LOGGING) {
                e.printStackTrace();
            }
            process = null;
        }
        return process;
    }

    private static <T> T log(T msg) {
        if (LOGGING) {
            System.err.println(msg);
        }
        return msg;
    }

    private static Map<String, JSONValue> callRPC(String method, Writer processIn, BufferedReader processOut, int id)
            throws IOException {
        return callRPC(method, null, processIn, processOut, id);
    }

    private static Map<String, JSONValue> callRPC(String method, Object params, Writer processIn,
            BufferedReader processOut, int id) throws IOException {
        String json;
        Map<String, JSONValue> obj = null;

        json = toRPC(id, method, params);
        processIn.write(json);
        processIn.write('\n');
        processIn.flush();

        // id >= 0 signifies a request (and we should get a response).
        if (id >= 0) {
            String reply = processOut.readLine();
            log("reply: " + reply);

            JSONValue value = JSONParser.create().parse(reply);
            log("value: " + value);
            obj = value.asObject();
            if (obj != null) {
                JSONValue result = obj.get("result");
                JSONValue error = obj.get("error");
                if (result != null) {
                    log("result: " + result);
                } else if (error != null) {
                    log("error: " + error);
                } else {
                    // Malformed
                    log("!! Malformed reply: \"" + reply + "\"");
                }
            } else {
                // Unexpected
                log("!! Malformed reply: \"" + reply + "\"");
            }

        } else {
            // id < 0 signifies a notification (and we will not get a response)

        }
        if (obj == null) {
            return Collections.emptyMap();
        } else {
            return obj;
        }
    }

    /**
     * If reply is an error reply, return its "code" attribute as an integer, if
     * possible, otherwise return 0 (zero).
     *
     * @param reply
     *            may be null
     * @return
     */
    private static int errorCode(Map<String, JSONValue> reply) {
        final int defaultValue = 0;

        if (reply == null) {
            return defaultValue;
        }
        return reply
                //
                .getOrDefault("error", JSONValue.emptyObject())
                //
                .asObject(Collections.emptyMap())
                //
                .getOrDefault("code", JSONValue.emptyObject())
                //
                .asNumber(defaultValue).intValue();
    }

    /**
     * True when the RPC reply indicates an ordinary failure from Prolog, e.g. after
     * "once" or "call" requests.
     *
     * @param reply
     *            may be null
     * @return
     */
    private static boolean isFail(Map<String, JSONValue> reply) {
        return errorCode(reply) == PROLOG_ERROR_CODE_FAIL;
    }

    /**
     * True when the RPC reply indicates an exception from Prolog, e.g. after "once"
     * or "call" requests.
     *
     * @param reply
     *            may be null
     * @return
     */
    private static boolean isException(Map<String, JSONValue> reply) {
        return errorCode(reply) == PROLOG_ERROR_CODE_EXCEPTION;
    }

    private static JSONValue getExceptionMessage(Map<String, JSONValue> reply, JSONValue defaultValue) {
        Map<String, JSONValue> erroObject = reply
                //
                .getOrDefault("error", JSONValue.emptyObject())
                //
                .asObject(Collections.emptyMap());

        if (erroObject.getOrDefault("code", JSONValue.emptyObject()).asNumber(0)
                .intValue() == PROLOG_ERROR_CODE_EXCEPTION) {
            return erroObject.getOrDefault("message", defaultValue);
        }

        return defaultValue;
    }

    private static JSONValue getExceptionData(Map<String, JSONValue> reply, JSONValue defaultValue) {
        Map<String, JSONValue> erroObject = reply
                //
                .getOrDefault("error", JSONValue.emptyObject())
                //
                .asObject(Collections.emptyMap());

        if (erroObject.getOrDefault("code", JSONValue.emptyObject()).asNumber(0)
                .intValue() == PROLOG_ERROR_CODE_EXCEPTION) {
            return erroObject.getOrDefault("data", defaultValue);
        }

        return defaultValue;
    }

    /**
     * True when the RPC reply indicates a successful reply from from Prolog, e.g.
     * after "once" or "call" requests.
     *
     * @param reply
     *            may be null
     * @return
     */
    private static boolean isSuccess(Map<String, JSONValue> reply) {
        return reply != null && reply.get("result") != null;
    }

    public static void main(String[] args) throws IOException, InterruptedException {

        if (RUN_TESTS) {
            int failures = 0;
            failures += testJSONParser();
            failures += testJSONWriter();
            if (failures > 0) {
                System.err.println("TEST FAILED: " + failures + " tests failed");
                System.exit(2);
            }
            return;
        }

        String exe_path = (args.length > 0 ? args[0] : "sicstus");
        String code_path = (args.length > 1 ? args[1] : "$SP_LIBRARY_DIR/json/examples/jsonrpc_server.pl");

        Process process = launch(exe_path, code_path);

        if (process != null) {
            BufferedReader processOut = new BufferedReader(
                    new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8));
            Writer processIn = new BufferedWriter(
                    new OutputStreamWriter(process.getOutputStream(), StandardCharsets.UTF_8));
            interact(processIn, processOut);

            process.waitFor();
        } else {
            System.err.println("Could not launch " + exe_path);
            System.exit(1);
        }

    }

    private static void interact(Writer processIn, BufferedReader processOut) throws IOException {
        int id = 0;

        Map<String, JSONValue> reply;
        Map<String, Object> params;

        // Read the initial state (it is null/Node).
        {
            System.err.print("state ==> ");
            reply = callRPC("state", processIn, processOut, id++);
            System.err.println("State is: " + reply.get("result"));
        }

        // Set the state (to 4) and return its previous value
        {
            System.err.print("state:=4 ==> ");
            reply = callRPC("state", Arrays.asList(4), processIn, processOut, id++);
            System.err.println("State was: " + reply.get("result"));
        }
        // Read the current state (it is 4).
        {
            System.err.print("state ==> ");
            reply = callRPC("state", processIn, processOut, id++);
            System.err.println("State is: " + reply.get("result"));
        }

        System.err.println("");
        // Increment current state by 1.
        {
            String goal = "Result is StateIn+1, StateOut=Result.";
            System.err.print("once " + goal + " ==> ");
            reply = callRPC("once", Collections.singletonMap("goal", goal), processIn, processOut, id++);
            System.err.println("Result=" + reply.get("result"));
        }

        // Increment current state by 1 (again).
        {
            String goal = "Result is StateIn+1, StateOut=Result.";
            System.err.print("once " + goal + " ==> ");
            reply = callRPC("once", Collections.singletonMap("goal", goal), processIn, processOut, id++);
            System.err.println("Result=" + reply.get("result"));
        }

        System.err.println("");
        // Increment the state with an increment specified as a VariableName:Value pair
        {
            String goal = "Result is StateIn+Increment,StateOut=Result.";
            System.err.print("once " + goal + " ==> ");
            params = new LinkedHashMap<>();
            params.put("goal", goal);
            params.put("bindings", Collections.singletonMap("Increment", 5));
            reply = callRPC("once", params, processIn, processOut, id++);
            System.err.println("Result=" + reply.get("result"));
        }

        System.err.println("");
        // Call member(...), backtracking over solutions
        {
            String goal = "member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc.";
            System.err.print("call " + "Multiplier=10, " + goal + " ==> ");

            params = new LinkedHashMap<>();
            params.put("goal", goal);
            params.put("bindings", Collections.singletonMap("Multiplier", 10));
            reply = callRPC("call", params, processIn, processOut, id++);
            System.err.println("First Result=" + reply.get("result"));
        }

        // Ask for the next solution
        {
            System.err.print("retry ==> ");
            reply = callRPC("retry", processIn, processOut, id++);
            System.err.println("(next) Result=" + reply.get("result"));
        }

        // Cut, committing to the last solution
        {
            System.err.print("cut ==> ");
            reply = callRPC("cut", processIn, processOut, id++);
            log(reply);
            System.err.println("Result=" + reply.get("result"));
        }

        System.err.println("");
        // Backtrack until failure
        {
            String goal = "member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc.";
            System.err.print("call " + "Multipleir=10," + goal + " ==> ");
            params = new LinkedHashMap<>();
            params.put("goal", goal);
            params.put("bindings", Collections.singletonMap("Multiplier", 10));
            reply = callRPC("call", params, processIn, processOut, id++);
            System.err.println("First Result=" + reply.get("result"));
        }
        // Ask for the next solution
        {
            System.err.print("retry ==> ");
            reply = callRPC("retry", processIn, processOut, id++);
            System.err.println("(next) Result=" + reply.get("result"));
        }
        // Ask for the next solution
        {
            System.err.print("retry ==> ");
            reply = callRPC("retry", processIn, processOut, id++);
            System.err.println("(next) Result=" + reply.get("result"));
        }

        // Ask for the next solution (this will fail, since there are only 3 elements in
        // the list)
        {
            System.err.print("retry ==> ");
            reply = callRPC("retry", processIn, processOut, id++);

            if (isSuccess(reply)) {
                System.err.println("(next) Result=" + reply.get("result"));
            } else if (isFail(reply)) {
                System.err.println("(next) Result failed (this is expected)");
            }
        }

        System.err.println("");
        // once(foo is bar). This will throw an exception in Prolog, which translates
        // into an error reply.
        {
            String goal = "foo is bar.";
            System.err.print("once " + goal + " ==> ");
            reply = callRPC("once", Collections.singletonMap("goal", goal), processIn, processOut, id++);
            if (isSuccess(reply)) {
                System.err.println("Result=" + reply.get("result"));
            } else if (isFail(reply)) {
                System.err.println("Result failed");
            } else if (isException(reply)) {
                System.err.println("Result threw exception (this is expected) "
                        + getExceptionData(reply, getExceptionMessage(reply, null)));
            }
        }

        System.err.println("");
        // Quit the server.
        {
            System.err.print("quit ==> ");
            reply = callRPC("quit", processIn, processOut, id++);
            if (isSuccess(reply)) {
                System.err.println("Result=" + reply.get("result"));
            } else if (isFail(reply)) {
                System.err.println("Result failed");
            } else if (isException(reply)) {
                System.err
                        .println("Result threw exception=" + getExceptionData(reply, getExceptionMessage(reply, null)));
            }
        }
    }
}
