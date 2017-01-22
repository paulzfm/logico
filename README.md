# logico

## Syntax

### Database

```
<rule> ::= <predicate>.
        |  <predicate> if <predicate-list>.

<predicate-list> ::= <predicate>
                  |  <predicate-list> and <predicate>

<predicate> ::= <word>
             |  <word>(<term-list>)

<term-list> ::= <term>
             |  <term-list>, <term>

<term> ::= <word>
        |  <variable>
        |  <integer>
        |  <list>

<list> ::= []
        |  [<term-list>]
```

### Query

```
<query> ::= load <database-name>.
         |  <predicate-list>
```
