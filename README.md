# bb4-expression

📊 [Build status for all bb4 projects](https://github.com/bb4/.github)

This library project for parsing mathematical expressions.

### Language (informal grammar)

`MathExpressionParser` accepts a single indeterminate `x`, decimal literals, parentheses, the binary operators `+ - * / ^`, optional whitespace, and implicit multiplication (e.g. `2x`, `2(x+1)`, `(x+1)x`). Unary `-` is supported for literals and for `x` (rendered as `-1 * x`). Other letters are rejected.

```ebnf
expr        = sum ;
sum         = product { ( '+' | '-' ) product } ;
product     = power { ( '*' | '/' ) power | implicit } ;
implicit    = /* juxtaposition after ')' or between factors: 2(x), (x+1)x */ ;
power       = atom { '^' atom } ;   (* note: '^' is left-associ here, not right-assoc *)
atom        = number | 'x' | '(' expr ')' | '-' atom ;
number      = digit { digit } [ '.' digit { digit } ] ;
```

Parse failures are thrown as `com.barrybecker4.expression.ParseError` (an `IllegalArgumentException` with `offset` into the trimmed source). Use `com.barrybecker4.expression.ExprEvaluator` to evaluate a parsed `Expr` for a numeric `x`.

To use, include this dependency in your gradle file

`compile 'com.barrybecker4:bb4-expression:<version>`

### [How to Build](https://github.com/barrybecker4/bb4-common/wiki/Building-bb4-Projects)
If you have not already done so, first install [Git](http://git-scm.com/) and [Intellij](http://www.jetbrains.com/idea/).
You also need to have [java SDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html) installed and set the JAVA_HOME environment variable to point to the location where it is installed.

Type 'gradlew build' at the root (prepend with ./ if running on Cygwin or Linux). This will build everything, but since it is a library project there won't be much to see. 

When there is a new release, versioned artifacts will be published by Barry Becker to [Sonatype](https://oss.sonatype.org).

### License

All source (unless otherwise specified in individual file) is provided under the [MIT License](http://www.opensource.org/licenses/MIT)
