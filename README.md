carma-models
============

Define model as ADT, where every member is "Field", then make instance.
Such model can be serialized into JSON, Redis Map and Postgresql row.

<pre>
data My k = My {
    myInt :: Field k Int,
    myString :: Field k String }
        deriving (Generic)

instance Model My where
    modelTable _ = "mytbl"
</pre>

Object of model <code>My</code>: each field is value of its type, <code>Int</code> and <code>String</code> in example

<pre>
test :: My Object
test = My 10 "Hello!"
</pre>

Patch: each field is of type <code>OptField</code>, where <code>Has</code> means, that field will be updated.
<code>testp</code> is patch, which will update <code>String</code> field:

<pre>
testp :: My Patch
testp = My HasNo (Has "World!")
</pre>

Usage:

<pre>
main :: IO ()
main = do
    either (const $ return ()) C8.putStrLn $ encodeJSON test
    -- {"myInt":10,"myString":"Hello!"}
    either (const $ return ()) C8.putStrLn $ encodeJSON testp
    -- {"myInt":null,"myString":"World!"}
    either (const $ return ()) print $ encodeRedis test
    -- fromList [("myInt","10"),("myString","\"Hello!\"")]
    con &lt;- connect testcon
    create con (Table :: Table (My Object))
    insert con (My 0 "hello" :: My Object)
    update_ con (My HasNo (Has "new") :: My Patch) " where myint = 0"
    v &lt;- select_ con "" :: IO [My Object]
    mapM_ (either (const $ return ()) C8.putStrLn . encodeJSON) v
    return ()
</pre>
