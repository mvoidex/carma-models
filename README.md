carma-models
============

Define model as ADT, where every member is "Field", then make instance.
Such model can be serializable into JSON, Redis Map and Postgresql row.

<pre>
data MyModel = MyModel {
	field1 :: Field Int,
	field2 :: Field String,
	field3 :: Field Double }
		deriving (Eq, Ord, Read, Show)

$(makeIso "myModel" ''MyModel)

instance Monoid MyModel where
	mempty = memptyIso myModel
	mappend = mappendIso myModel

instance Model MyModel where
	asDict =
		field "field1" "This field is number 1" .**.
		field "field2" "This is second field" .**.
		field "field3" "And the last"
		.:.
		myModel
</pre>

To get description:

<pre>
myInfo :: Map Text Text -- field -&gt; desc
myInfo = modelInfo (undefined :: MyModel)
</pre>

To encode:

<pre>
test = MyModel (mkField 1) (mkField "some string") (mkField 1.2)

jsoned :: Either String ByteString
jsoned = encodeModel test

redised :: Either String (Map ByteString ByteString)
redised = encodeModel test
</pre>

To decode use similar 'decodeModel' function.
