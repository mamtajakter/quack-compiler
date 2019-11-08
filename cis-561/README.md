# An implementation of the [Quack](https://piazza-resources.s3.amazonaws.com/ixph1tsperz3b/iyc8hprr96m76w/quack_grammar.pdf?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=ASIAR6AWVCBX6IXPTP3M%2F20181214%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20181214T231039Z&X-Amz-Expires=3600&X-Amz-SignedHeaders=host&X-Amz-Security-Token=FQoGZXIvYXdzEND%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaDK6FlHvuPefEDP75yyK3A%2BFy71OdNdStKK18JG5nHpkGecyM1bb%2Foo7uqrIRgZ1RpXquhcjEaob%2Fwtlo6dKK3uRQsLubtNJRAfoX1j1gPbnjUAf1riQUFwjJmErIKU6V62ZTvEDpTVYmAijgsz%2Ff93ewEaxRhxVMzScm9PrbqUA9%2FPbuNRXy7L2gWh7Ce%2BWnFl4iM%2BAsU%2B1x6N1wlfjNUnHFpb6GFIRGiN%2BRZhpYYBcGtWJz1wt8n3KUcWkLGldCxEwCRGAPHyG8W0m3xn5GciKnWB1M0OWaFQGAS454bT8OEjEOIhaV6CKquSDuF3iKlE%2Fkvv6Guzr5dCnc6Nj6IZHaabAAI1%2FWNhos53v1vl4uUIXzMofc36MVqQ%2B0aSsekS%2F9BlFy9dvSc1gefRywcujLcOB2v40NkiD4Due0OVTkrXJLH0iuLbin75wbXW7Q6m0b7Ot0FaL2L9%2FjCkKUomZmkoutZA2FGl27ZAbJl8PH8Y1FRpk14G5eZxAfJHV67bzSDOjPQjhA%2B6qdRrguHZZbAhIDhGf%2FbuDmpCkks5B%2BKk%2FpdXHoYf2J%2BR%2B9iG3tqzkQrhsZCA1Eh4SM0z1%2FMgp7XG9xKREo4d7Q4AU%3D&X-Amz-Signature=25857d3a8a9c3a901cfd0d71ddb1eaef1b29bf94db17e54d8a81b6b9f2a057a9) OO language


The implementation of the lexer closely follows that of https://github.com/simonmar/alex/blob/master/examples/tiger.x

- Type checker implementation complete!

- the code generator works for statements, control flow statements, and simple class hierarchies 

- Check demo folder for examples of some programs that were able to compile

- Code generator generates C code.


# To run the compiler
```
  - cd quack
  - make quack
  - stack exec quack <input-quack-file> <output-c-file>
  - gcc <output-c-file>
```
  
