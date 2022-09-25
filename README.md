#About

CBreak is language in the C family designed to have controlled periodic breaking changes such that it can evolve as times change.  It is inspired by C++, though not source compatible with C or C++.

See docs/language0.tex for details.

The language is still in early development.  Many of the documented features have not been implemented yet, and large portions of the design are still incomplete.


#Example
```cpp
#export (#int32) main ()
{
    #var SomeClass c;
    c.SomeMutable = 7;

    #return c.AddSomething();
}

#class SomeClass
{
    #const #int32 SomeConstant = 42;

    #var #int32 SomeMutable = 13;

    (#int32 ret) AddSomething (#int32 extraValue)
    {
        ret = #this.SomeConstant + #this.SomeMutable + extraValue;
        #return; //needed on the current implementation - should be omittable later
    }
}
```
