| package |
package := Package name: 'MessagePack-Core'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #MpConstants;
	add: #MpDecoder;
	add: #MpDecodeTypeMapper;
	add: #MpEncoder;
	add: #MpEncodeTypeMapper;
	add: #MpError;
	add: #MpMessagePack;
	add: #MpPortableUtil;
	add: #MpSettings;
	add: #MpTypeMapper;
	yourself.

package methodNames
	add: #Behavior -> #fromMessagePack:;
	add: #Object -> #messagePacked;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #MpConstants
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MpDecoder
	instanceVariableNames: 'readStream typeMapper settings'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MpEncoder
	instanceVariableNames: 'writeStream typeMapper settings'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MpMessagePack
	instanceVariableNames: ''
	classVariableNames: 'Default DialectSpecificClass'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MpPortableUtil
	instanceVariableNames: ''
	classVariableNames: 'Default DialectSpecificClass'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MpSettings
	instanceVariableNames: 'settingsDict'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MpTypeMapper
	instanceVariableNames: 'actionMap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'actionMap'!
Error subclass: #MpError
	instanceVariableNames: 'type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MpTypeMapper subclass: #MpDecodeTypeMapper
	instanceVariableNames: 'decoder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MpTypeMapper subclass: #MpEncodeTypeMapper
	instanceVariableNames: 'encoder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Behavior methodsFor!

fromMessagePack: bytes	^ MpDecoder decode: bytes! !
!Behavior categoriesFor: #fromMessagePack:!*MessagePack/Core/unpacking!public! !

!Object methodsFor!

messagePacked	^ MpEncoder encode: self! !
!Object categoriesFor: #messagePacked!*MessagePack/Core/packing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

MpConstants guid: (GUID fromString: '{73C91E8E-85C6-43A9-A947-AE9A782C33D4}')!
MpConstants comment: ''!
!MpConstants categoriesForClass!MessagePack-Core! !
!MpConstants class methodsFor!

array16	^16rDC!

array32	^16rDD!

double	^16rCB!

false	^16rC2!

float	^16rCA!

int16	^16rD1!

int32	^16rD2!

int64	^16rD3!

int8	^16rD0!

map16	^16rDE!

map32	^16rDF!

nil	^16rC0!

raw16	^16rDA!

raw32	^16rDB!

true	^16rC3!

uint16	^16rCD!

uint32	^16rCE!

uint64	^16rCF!

uint8	^16rCC! !
!MpConstants class categoriesFor: #array16!public!typecodes! !
!MpConstants class categoriesFor: #array32!public!typecodes! !
!MpConstants class categoriesFor: #double!public!typecodes! !
!MpConstants class categoriesFor: #false!public!typecodes! !
!MpConstants class categoriesFor: #float!public!typecodes! !
!MpConstants class categoriesFor: #int16!public!typecodes! !
!MpConstants class categoriesFor: #int32!public!typecodes! !
!MpConstants class categoriesFor: #int64!public!typecodes! !
!MpConstants class categoriesFor: #int8!public!typecodes! !
!MpConstants class categoriesFor: #map16!public!typecodes! !
!MpConstants class categoriesFor: #map32!public!typecodes! !
!MpConstants class categoriesFor: #nil!public!typecodes! !
!MpConstants class categoriesFor: #raw16!public!typecodes! !
!MpConstants class categoriesFor: #raw32!public!typecodes! !
!MpConstants class categoriesFor: #true!public!typecodes! !
!MpConstants class categoriesFor: #uint16!public!typecodes! !
!MpConstants class categoriesFor: #uint32!public!typecodes! !
!MpConstants class categoriesFor: #uint64!public!typecodes! !
!MpConstants class categoriesFor: #uint8!public!typecodes! !

MpDecoder guid: (GUID fromString: '{89723DAD-1204-43F6-9F3D-47EDE62D693E}')!
MpDecoder comment: ''!
!MpDecoder categoriesForClass!MessagePack-Core! !
!MpDecoder methodsFor!

atEnd	^self readStream atEnd!

createArray: size	^Array new: size!

createDictionary: size	^Dictionary new: size!

createOrderedCollection: size	^OrderedCollection new: size!

decode	self readStream atEnd ifTrue: [^self signalError: 'No data to read'].	^self read!

decode: byteArray	^self decodeFrom: byteArray readStream!

decodeFrom: aStream	self readStream: aStream "binary".	^self decode!

next	^self read!

read	^self readObject.	!

readArray16	| size |	size := MpPortableUtil default readUint16From: self readStream.	^ self readArraySized: size!

readArray32	| size |	size := MpPortableUtil default readUint32From: self readStream.	^ self readArraySized: size!

readArraySized: size	| array |	array := self createArray: size.	1 to: size do: [:idx |		array at: idx put: (self readObject)	].	^array!

readDouble	"64 bit"	^MpPortableUtil default readDoubleFrom: self readStream!

readFalse	^false!

readFixArray: firstByte	| size |	size := (firstByte bitAnd: 2r1111).	^ self readArraySized: size!

readFixMap: firstByte	| size |	size := (firstByte bitAnd: 2r1111).	^self readMapSized: size!

readFixRaw: firstByte	| size |	size := (firstByte bitAnd: 2r11111).	^self readStream next: size!

readFloat	"32 bit"	^MpPortableUtil default readFloatFrom: self readStream	!

readInt16	^ MpPortableUtil default readInt16From: self readStream!

readInt32	^ MpPortableUtil default readInt32From: self readStream!

readInt64	^ MpPortableUtil default readInt64From: self readStream!

readInt8	| val |	val := self readStream next.	val >= 128 ifTrue: [^(256 - val) negated].	^val!

readMap16	| size |	size := MpPortableUtil default readUint16From: self readStream.	^ self readMapSized: size!

readMap32	| size |	size := MpPortableUtil default readUint32From: self readStream.	^ self readMapSized: size!

readMapSized: size	| dic |	dic := self createDictionary: size.	1 to: size do: [:idx |		dic at: self readObject put: self readObject	].	^dic!

readNegativeFixNum: firstByte	| val |	val := (firstByte bitAnd: 2r11111).	^ val - 32!

readNil	^nil!

readObject	| type |	type := self readType.	^ self readObjectOf: type	!

readObjectOf: type	^ self readObjectOf: type ifNotApplied: [self signalError]		!

readObjectOf: type ifNotApplied: aBlock		type <=16rBF ifTrue: [ | fixMapOrArray |		type <= 16r7F ifTrue: [^ self readPositiveFixNum: type].		fixMapOrArray := type bitShift: -4.		fixMapOrArray = 2r1000 ifTrue: [^ self readFixMap: type].		fixMapOrArray = 2r1001 ifTrue: [^ self readFixArray: type].		^ self readFixRaw: type.	].	(type bitShift: -5) = 2r111 ifTrue: [^ self readNegativeFixNum: type].	^self typeMapper readObjectOf: type ifNotApplied: aBlock!

readPositiveFixNum: firstByte	"0 - 127"	^ firstByte!

readRaw16	| size |	size := MpPortableUtil default readUint16From: self readStream.	^self readStream next: size!

readRaw32	| size |	size := MpPortableUtil default readUint32From: self readStream.	^self readStream next: size!

readStream	"Answer the value of readStream"	^ readStream!

readStream: anObject	"Set the value of readStream"	readStream := anObject!

readTrue	^true!

readType	^self readStream next!

readUint16	^ MpPortableUtil default readUint16From: self readStream!

readUint32	^ MpPortableUtil default readUint32From: self readStream!

readUint64	^ MpPortableUtil default readUint64From: self readStream!

readUint8	^ self readStream next!

settings	^settings ifNil: [settings := self settingsClass new]!

settingsClass	^MpSettings!

signalError	self signalError: 'Cannot decode'!

signalError: message	^MpPortableUtil default signalException: (MpError decode messageText: message) !

typeMapper	^ typeMapper ifNil: [typeMapper := self typeMapperClass on: self].!

typeMapperClass	^MpDecodeTypeMapper! !
!MpDecoder categoriesFor: #atEnd!public!stream/like! !
!MpDecoder categoriesFor: #createArray:!factory!public! !
!MpDecoder categoriesFor: #createDictionary:!factory!public! !
!MpDecoder categoriesFor: #createOrderedCollection:!factory!public! !
!MpDecoder categoriesFor: #decode!decoding!public! !
!MpDecoder categoriesFor: #decode:!decoding!public! !
!MpDecoder categoriesFor: #decodeFrom:!decoding!public! !
!MpDecoder categoriesFor: #next!public!stream/like! !
!MpDecoder categoriesFor: #read!public!reading! !
!MpDecoder categoriesFor: #readArray16!public!reading! !
!MpDecoder categoriesFor: #readArray32!public!reading! !
!MpDecoder categoriesFor: #readArraySized:!public!reading/helper! !
!MpDecoder categoriesFor: #readDouble!public!reading! !
!MpDecoder categoriesFor: #readFalse!public!reading! !
!MpDecoder categoriesFor: #readFixArray:!public!reading! !
!MpDecoder categoriesFor: #readFixMap:!public!reading! !
!MpDecoder categoriesFor: #readFixRaw:!public!reading! !
!MpDecoder categoriesFor: #readFloat!public!reading! !
!MpDecoder categoriesFor: #readInt16!public!reading/integer! !
!MpDecoder categoriesFor: #readInt32!public!reading/integer! !
!MpDecoder categoriesFor: #readInt64!public!reading/integer! !
!MpDecoder categoriesFor: #readInt8!public!reading/integer! !
!MpDecoder categoriesFor: #readMap16!public!reading! !
!MpDecoder categoriesFor: #readMap32!public!reading! !
!MpDecoder categoriesFor: #readMapSized:!public!reading/helper! !
!MpDecoder categoriesFor: #readNegativeFixNum:!public!reading! !
!MpDecoder categoriesFor: #readNil!public!reading! !
!MpDecoder categoriesFor: #readObject!dispatching!public! !
!MpDecoder categoriesFor: #readObjectOf:!dispatching!public! !
!MpDecoder categoriesFor: #readObjectOf:ifNotApplied:!dispatching!public! !
!MpDecoder categoriesFor: #readPositiveFixNum:!public!reading! !
!MpDecoder categoriesFor: #readRaw16!public!reading! !
!MpDecoder categoriesFor: #readRaw32!public!reading! !
!MpDecoder categoriesFor: #readStream!accessing!public! !
!MpDecoder categoriesFor: #readStream:!accessing!public! !
!MpDecoder categoriesFor: #readTrue!public!reading! !
!MpDecoder categoriesFor: #readType!public!reading/helper! !
!MpDecoder categoriesFor: #readUint16!public!reading/integer! !
!MpDecoder categoriesFor: #readUint32!public!reading/integer! !
!MpDecoder categoriesFor: #readUint64!public!reading/integer! !
!MpDecoder categoriesFor: #readUint8!public!reading/integer! !
!MpDecoder categoriesFor: #settings!accessing!public! !
!MpDecoder categoriesFor: #settingsClass!factory!public! !
!MpDecoder categoriesFor: #signalError!public!signaling error! !
!MpDecoder categoriesFor: #signalError:!public!signaling error! !
!MpDecoder categoriesFor: #typeMapper!accessing!public! !
!MpDecoder categoriesFor: #typeMapperClass!factory!public! !

!MpDecoder class methodsFor!

decode: byteArray	^self new decode: byteArray!

decodeFrom: aStream	^self new decodeFrom: aStream!

on: aStream	^self new readStream: aStream "binary"!

onBytes: byteArray	^self on: (ReadStream on: byteArray)! !
!MpDecoder class categoriesFor: #decode:!actions!public! !
!MpDecoder class categoriesFor: #decodeFrom:!actions!public! !
!MpDecoder class categoriesFor: #on:!actions!public! !
!MpDecoder class categoriesFor: #onBytes:!actions!public! !

MpEncoder guid: (GUID fromString: '{20B63E24-CF65-4958-8522-56F3122EA482}')!
MpEncoder comment: ''!
!MpEncoder categoriesForClass!MessagePack-Core! !
!MpEncoder methodsFor!

contents	^self writeStream contents!

encode: anObject	^self encode: anObject on: self writeStream!

encode: anObject on: aStream	self write: anObject on: aStream.	^self contents.!

nextPut: anObject	self writeObject: anObject!

nextPutAll: aCollection	aCollection do: [:each | self nextPut: each]!

settings	^settings ifNil: [settings := self settingsClass new]!

settingsClass	^MpSettings!

signalError	self signalError: 'Cannot encode'!

signalError: message	^MpPortableUtil default signalException: (MpError encode messageText: message) !

typeMapper	^ typeMapper ifNil: [typeMapper := self typeMapperClass on: self].!

typeMapperClass	^MpPortableUtil default encodeTypeMapperClass!

write: anObject on: aStream	self writeStream: aStream "binary".	self writeObject: anObject!

writeArray: array	| size |	size := array size.	self writeArraySize: size.	array do: [:each | self writeObject: each].	!

writeArraySize: size	size < 16r10 ifTrue: [^ self writeStream nextPut: (2r10010000 bitOr: size)].	size < 16r10000  ifTrue: [		self writeStream nextPut: MpConstants array16.		^MpPortableUtil default writeUint16: size to: self writeStream	].	size < 16r100000000  ifTrue: [		self writeStream nextPut: MpConstants array32.		^MpPortableUtil default writeUint32: size to: self writeStream	].		self signalError!

writeDouble: aFloat	self writeStream nextPut: MpConstants double.	MpPortableUtil default writeDouble: aFloat to: self writeStream!

writeFalse: ignore	self writeStream nextPut: MpConstants false!

writeFloat: aFloat	self writeStream nextPut: MpConstants float.	MpPortableUtil default writeFloat: aFloat to: self writeStream!

writeInt16: value	self writeStream nextPut: MpConstants int16.	MpPortableUtil default writeInt16: value to: self writeStream!

writeInt32: value	self writeStream nextPut: MpConstants int32.	MpPortableUtil default writeInt32: value to: self writeStream!

writeInt64: value	self writeStream nextPut: MpConstants int64.	MpPortableUtil default writeInt64: value to: self writeStream!

writeInt8: value	| val |	self writeStream nextPut: MpConstants int8.	val := value < 0 ifTrue: [256 + value] ifFalse: [value].	self writeStream nextPut: val!

writeInteger: anInteger 	(anInteger between: 0 and: 127)		ifTrue: [^self writePositiveFixNum: anInteger].			(anInteger between: -32 and: -1)		ifTrue: [^self writeNegativeFixNum: anInteger].		anInteger >= 128 ifTrue: [		anInteger <= 255 ifTrue: [^self writeUint8: anInteger].		anInteger <= 65535 ifTrue: [^self writeUint16: anInteger].		anInteger <= 4294967295 ifTrue: [^self writeUint32: anInteger].		anInteger <= 18446744073709551615 ifTrue: [^self writeUint64: anInteger].	].		anInteger >= -128 ifTrue: [^self writeInt8: anInteger].	anInteger >= -32768 ifTrue: [^self writeInt16: anInteger].	anInteger >= -2147483648 ifTrue: [^self writeInt32: anInteger].	anInteger >= -9223372036854775808 ifTrue: [^self writeInt64: anInteger].		self signalError!

writeMap: aDictionary	| size |	size := aDictionary size.	self writeMapSize: size.	aDictionary keysAndValuesDo: [:key :value | self writeObject: key; writeObject: value].!

writeMapSize: size	size < 16r10 ifTrue: [^ self writeStream nextPut: (2r10000000 bitOr: size)].	size < 16r10000  ifTrue: [		self writeStream nextPut: MpConstants map16.		^MpPortableUtil default writeUint16: size to: self writeStream	].	size < 16r100000000  ifTrue: [		self writeStream nextPut: MpConstants map32.		^MpPortableUtil default writeUint32: size to: self writeStream	].		self signalError!

writeNegativeFixNum: number	"-32 to -1"	| val |	val :=  256 + number.	self writeStream nextPut: val!

writeNil: ignore	self writeStream nextPut: MpConstants nil!

writeObject: anObject 	^self		writeObject: anObject		ifNotApplied: [self signalError]!

writeObject: anObject ifNotApplied: aBlock 	^self typeMapper writeObject: anObject ifNotApplied: aBlock!

writePositiveFixNum: number	self writeStream nextPut: number	!

writeRawBytes: bytes	| size |	size := bytes size.	self writeRawBytesSize: size.	self writeStream nextPutAll: bytes!

writeRawBytesSize: size	size < 16r10 ifTrue: [^ self writeStream nextPut: (2r10100000 bitOr: size)].	size < 16r10000  ifTrue: [		self writeStream nextPut: MpConstants raw16.		^MpPortableUtil default writeUint16: size to: self writeStream	].	size < 16r100000000  ifTrue: [		self writeStream nextPut: MpConstants raw32.		^MpPortableUtil default writeUint32: size to: self writeStream	].		self signalError!

writeStream	writeStream isNil		ifTrue: [writeStream := WriteStream						on: (ByteArray new: self settings defaultStreamSize)].	^ writeStream!

writeStream: anObject	"Set the value of writeStream"	writeStream := anObject!

writeTrue: ignore	self writeStream nextPut: MpConstants true!

writeUint16: value	self writeStream nextPut: MpConstants uint16.	MpPortableUtil default writeUint16: value to: self writeStream!

writeUint32: value	self writeStream nextPut: MpConstants uint32.	MpPortableUtil default writeUint32: value to: self writeStream!

writeUint64: value	self writeStream nextPut: MpConstants uint64.	MpPortableUtil default writeUint64: value to: self writeStream!

writeUint8: value	self writeStream nextPut: MpConstants uint8.	self writeStream nextPut: value! !
!MpEncoder categoriesFor: #contents!accessing!public! !
!MpEncoder categoriesFor: #encode:!encoding!public! !
!MpEncoder categoriesFor: #encode:on:!encoding!public! !
!MpEncoder categoriesFor: #nextPut:!public!stream/like! !
!MpEncoder categoriesFor: #nextPutAll:!public!stream/like! !
!MpEncoder categoriesFor: #settings!accessing!public! !
!MpEncoder categoriesFor: #settingsClass!factory!public! !
!MpEncoder categoriesFor: #signalError!public!signaling error! !
!MpEncoder categoriesFor: #signalError:!public!signaling error! !
!MpEncoder categoriesFor: #typeMapper!accessing!public! !
!MpEncoder categoriesFor: #typeMapperClass!accessing!public! !
!MpEncoder categoriesFor: #write:on:!encoding!public! !
!MpEncoder categoriesFor: #writeArray:!public!writing! !
!MpEncoder categoriesFor: #writeArraySize:!public!writing/helper! !
!MpEncoder categoriesFor: #writeDouble:!public!writing! !
!MpEncoder categoriesFor: #writeFalse:!public!writing! !
!MpEncoder categoriesFor: #writeFloat:!public!writing! !
!MpEncoder categoriesFor: #writeInt16:!public!writing/helper! !
!MpEncoder categoriesFor: #writeInt32:!public!writing/helper! !
!MpEncoder categoriesFor: #writeInt64:!public!writing/helper! !
!MpEncoder categoriesFor: #writeInt8:!public!writing/helper! !
!MpEncoder categoriesFor: #writeInteger:!public!writing! !
!MpEncoder categoriesFor: #writeMap:!public!writing! !
!MpEncoder categoriesFor: #writeMapSize:!public!writing/helper! !
!MpEncoder categoriesFor: #writeNegativeFixNum:!public!writing/helper! !
!MpEncoder categoriesFor: #writeNil:!public!writing! !
!MpEncoder categoriesFor: #writeObject:!dispatching!public! !
!MpEncoder categoriesFor: #writeObject:ifNotApplied:!dispatching!public! !
!MpEncoder categoriesFor: #writePositiveFixNum:!public!writing/helper! !
!MpEncoder categoriesFor: #writeRawBytes:!public!writing! !
!MpEncoder categoriesFor: #writeRawBytesSize:!public!writing/helper! !
!MpEncoder categoriesFor: #writeStream!accessing!public! !
!MpEncoder categoriesFor: #writeStream:!accessing!public! !
!MpEncoder categoriesFor: #writeTrue:!public!writing! !
!MpEncoder categoriesFor: #writeUint16:!public!writing/helper! !
!MpEncoder categoriesFor: #writeUint32:!public!writing/helper! !
!MpEncoder categoriesFor: #writeUint64:!public!writing/helper! !
!MpEncoder categoriesFor: #writeUint8:!public!writing/helper! !

!MpEncoder class methodsFor!

encode: anObject 	^self new encode: anObject!

encode: anObject on: aStream	^self new encode: anObject on: aStream!

on: aStream	^self new writeStream: aStream; yourself!

onBytes: byteArray	^self on: (WriteStream on: byteArray).! !
!MpEncoder class categoriesFor: #encode:!actions!public! !
!MpEncoder class categoriesFor: #encode:on:!actions!public! !
!MpEncoder class categoriesFor: #on:!actions!public! !
!MpEncoder class categoriesFor: #onBytes:!actions!public! !

MpMessagePack guid: (GUID fromString: '{31D67B60-74CB-481F-AA2E-CD18C230C365}')!
MpMessagePack comment: ''!
!MpMessagePack categoriesForClass!MessagePack-Core! !
!MpMessagePack class methodsFor!

pack: anObject	^ MpEncoder encode: anObject!

packUnpack: anObject	^self unpack: (self pack: anObject)!

unpack: aByteArray	^ MpDecoder decode: aByteArray! !
!MpMessagePack class categoriesFor: #pack:!public!utilities! !
!MpMessagePack class categoriesFor: #packUnpack:!public!utilities! !
!MpMessagePack class categoriesFor: #unpack:!public!utilities! !

MpPortableUtil guid: (GUID fromString: '{69E71C80-220C-4A66-8F33-675342359FAE}')!
MpPortableUtil comment: ''!
!MpPortableUtil categoriesForClass!MessagePack-Core! !
!MpPortableUtil methodsFor!

collectionEquals: aCollection with: otherCollection	"For testing"	^ aCollection = otherCollection!

encodeTypeMapperClass	^MpEncodeTypeMapper!

newCollection: aCollectionClass sized: size withAll: elem	"For testing"	^ aCollectionClass new: size withAll: elem!

randomClass	"For testing"	^Smalltalk at: #Random!

readDoubleFrom: stream	self subclassResponsibility !

readFloatFrom: stream	self subclassResponsibility !

readInt16From: stream	self subclassResponsibility !

readInt32From: stream	self subclassResponsibility !

readInt64From: stream	self subclassResponsibility !

readUint16From: stream	self subclassResponsibility !

readUint32From: stream	self subclassResponsibility !

readUint64From: stream	self subclassResponsibility !

signalException: anException	"Ansi"	^anException signal!

writeDouble: value to: stream	self subclassResponsibility !

writeFloat: value to: stream	self subclassResponsibility !

writeInt16: value to: stream	self subclassResponsibility !

writeInt32: value to: stream	self subclassResponsibility !

writeInt64: value to: stream	self subclassResponsibility !

writeUint16: value to: stream	self subclassResponsibility !

writeUint32: value to: stream	self subclassResponsibility !

writeUint64: value to: stream	self subclassResponsibility ! !
!MpPortableUtil categoriesFor: #collectionEquals:with:!public!testing! !
!MpPortableUtil categoriesFor: #encodeTypeMapperClass!factory!public! !
!MpPortableUtil categoriesFor: #newCollection:sized:withAll:!factory!public! !
!MpPortableUtil categoriesFor: #randomClass!factory!public! !
!MpPortableUtil categoriesFor: #readDoubleFrom:!actions/stream!public! !
!MpPortableUtil categoriesFor: #readFloatFrom:!actions/stream!public! !
!MpPortableUtil categoriesFor: #readInt16From:!actions/stream!public! !
!MpPortableUtil categoriesFor: #readInt32From:!actions/stream!public! !
!MpPortableUtil categoriesFor: #readInt64From:!actions/stream!public! !
!MpPortableUtil categoriesFor: #readUint16From:!actions/stream!public! !
!MpPortableUtil categoriesFor: #readUint32From:!actions/stream!public! !
!MpPortableUtil categoriesFor: #readUint64From:!actions/stream!public! !
!MpPortableUtil categoriesFor: #signalException:!actions!public! !
!MpPortableUtil categoriesFor: #writeDouble:to:!actions/stream!public! !
!MpPortableUtil categoriesFor: #writeFloat:to:!actions/stream!public! !
!MpPortableUtil categoriesFor: #writeInt16:to:!actions/stream!public! !
!MpPortableUtil categoriesFor: #writeInt32:to:!actions/stream!public! !
!MpPortableUtil categoriesFor: #writeInt64:to:!actions/stream!public! !
!MpPortableUtil categoriesFor: #writeUint16:to:!actions/stream!public! !
!MpPortableUtil categoriesFor: #writeUint32:to:!actions/stream!public! !
!MpPortableUtil categoriesFor: #writeUint64:to:!actions/stream!public! !

!MpPortableUtil class methodsFor!

default	^Default ifNil: [Default := self dialectSpecificClass new]!

dialectSpecificClass	^DialectSpecificClass ifNil: [DialectSpecificClass := self subclasses at: 1]!

dialectSpecificClass: aClass	DialectSpecificClass := aClass!

initialize	Default := nil.	DialectSpecificClass := nil!

stomp	^self default stompUtil! !
!MpPortableUtil class categoriesFor: #default!instance creation!public! !
!MpPortableUtil class categoriesFor: #dialectSpecificClass!factory!public! !
!MpPortableUtil class categoriesFor: #dialectSpecificClass:!factory!public! !
!MpPortableUtil class categoriesFor: #initialize!class initialization!public! !
!MpPortableUtil class categoriesFor: #stomp!*stomp/core!public! !

MpSettings guid: (GUID fromString: '{EF50FFC9-A990-41DF-80BA-9C82CE619756}')!
MpSettings comment: ''!
!MpSettings categoriesForClass!MessagePack-Core! !
!MpSettings methodsFor!

at: key	^self settingsDict at: key!

at: key ifAbsent: aBlock	^self settingsDict at: key ifAbsent: aBlock!

at: key ifAbsentPut: aBlock	^self settingsDict at: key ifAbsentPut: aBlock!

at: key put: value	^self settingsDict at: key put: value!

defaultStreamSize	^self at: #defaultStreamSize ifAbsentPut: [1024]!

defaultStreamSize: anInteger	^self at: #defaultStreamSize put: anInteger!

includesKey: key	^self settingsDict includesKey: key!

initialize	settingsDict := nil!

keys	^self settingsDict keys!

settingsDict	^ settingsDict ifNil: [settingsDict := IdentityDictionary new]! !
!MpSettings categoriesFor: #at:!actions/dictionary!public! !
!MpSettings categoriesFor: #at:ifAbsent:!actions/dictionary!public! !
!MpSettings categoriesFor: #at:ifAbsentPut:!actions/dictionary!public! !
!MpSettings categoriesFor: #at:put:!actions/dictionary!public! !
!MpSettings categoriesFor: #defaultStreamSize!accessing!public! !
!MpSettings categoriesFor: #defaultStreamSize:!accessing!public! !
!MpSettings categoriesFor: #includesKey:!actions/dictionary!public! !
!MpSettings categoriesFor: #initialize!class initialization!public! !
!MpSettings categoriesFor: #keys!actions/dictionary!public! !
!MpSettings categoriesFor: #settingsDict!accessing/private!public! !

MpTypeMapper guid: (GUID fromString: '{D65F572E-6BCB-4FBF-ABC9-2A71EF8796A8}')!
MpTypeMapper comment: ''!
!MpTypeMapper categoriesForClass!MessagePack-Core! !
!MpTypeMapper methodsFor!

actionMap	^ actionMap ifNil: [actionMap := IdentityDictionary new]!

defaultActionMap	^ self class actionMap!

initActionMaps	"override for custom mapping"	actionMap := nil! !
!MpTypeMapper categoriesFor: #actionMap!accessing!public! !
!MpTypeMapper categoriesFor: #defaultActionMap!accessing!public! !
!MpTypeMapper categoriesFor: #initActionMaps!initialization!public! !

!MpTypeMapper class methodsFor!

actionMap	^ actionMap ifNil: [actionMap := self createActionMap]!

createActionMap	| map |	map := IdentityDictionary new.	self definePrimitivesActionsTo: map.	self defineCompoundsActionsTo: map.	^map!

defineCompoundsActionsTo: map	"override"!

definePrimitivesActionsTo: map	"override"!

initialize	"self initialize"	actionMap  := nil.	self actionMap	!

initializeAll	"self initializeAll"	self allSubclasses do: [:each | each initialize]! !
!MpTypeMapper class categoriesFor: #actionMap!accessing!public! !
!MpTypeMapper class categoriesFor: #createActionMap!factory!public! !
!MpTypeMapper class categoriesFor: #defineCompoundsActionsTo:!actions for compounds!public! !
!MpTypeMapper class categoriesFor: #definePrimitivesActionsTo:!actions for primitives!public! !
!MpTypeMapper class categoriesFor: #initialize!class initialization!public! !
!MpTypeMapper class categoriesFor: #initializeAll!class initialization!public! !

MpError guid: (GUID fromString: '{1B68E4DB-4DCF-4B35-ADC9-5A637A9454B9}')!
MpError comment: ''!
!MpError categoriesForClass!MessagePack-Core! !
!MpError methodsFor!

type	"Answer the value of type"	^ type!

type: anObject	"Set the value of type"	type := anObject! !
!MpError categoriesFor: #type!accessing!public! !
!MpError categoriesFor: #type:!accessing!public! !

!MpError class methodsFor!

decode	^self new type: #decode!

encode	^self new type: #encode! !
!MpError class categoriesFor: #decode!instance creation!public! !
!MpError class categoriesFor: #encode!instance creation!public! !

MpDecodeTypeMapper guid: (GUID fromString: '{D7C2DF5D-3E36-44DC-A75D-CC93D84CBBC8}')!
MpDecodeTypeMapper comment: ''!
!MpDecodeTypeMapper categoriesForClass!MessagePack-Core! !
!MpDecodeTypeMapper methodsFor!

decoder	"Answer the value of decoder"	^ decoder!

decoder: anObject	"Set the value of decoder"	decoder := anObject!

readObjectOf: typeCode ifNotApplied: aBlock 	| actionSelector |		actionMap		ifNotNil: [actionSelector := self actionMap at: typeCode ifAbsent: [].			actionSelector ifNotNil: [^ self decoder perform: actionSelector]].		actionSelector := self defaultActionMap				at: typeCode				ifAbsent: [^ aBlock value].	^ self decoder perform: actionSelector! !
!MpDecodeTypeMapper categoriesFor: #decoder!accessing!public! !
!MpDecodeTypeMapper categoriesFor: #decoder:!accessing!public! !
!MpDecodeTypeMapper categoriesFor: #readObjectOf:ifNotApplied:!actions!public! !

!MpDecodeTypeMapper class methodsFor!

defineArrayActionTo: map	map at: MpConstants array16 put: #readArray16.	map at: MpConstants array32 put: #readArray32.		!

defineCompoundsActionsTo: map	self defineArrayActionTo: map.	self defineMapActionTo: map.!

defineDoubleActionTo: map	map at: MpConstants double put: #readDouble!

defineFalseActionTo: map	map at: MpConstants false put: #readFalse!

defineFloatActionTo: map	map at: MpConstants float put: #readFloat!

defineIntegerActionTo: map	map at: MpConstants int8 put: #readInt8.	map at: MpConstants int16 put: #readInt16.	map at: MpConstants int32 put: #readInt32.	map at: MpConstants int64 put: #readInt64.!

defineMapActionTo: map		map at: MpConstants map16 put: #readMap16.	map at: MpConstants map32 put: #readMap32.!

defineNilActionTo: map	map at: MpConstants nil put: #readNil!

definePrimitivesActionsTo: map	self defineNilActionTo: map.	self defineFalseActionTo: map.	self defineTrueActionTo: map.	self defineFloatActionTo: map.	self defineDoubleActionTo: map.	self defineUnsignedIntegerActionTo: map.	self defineIntegerActionTo: map.	self defineRawBytesActionTo: map.	!

defineRawBytesActionTo: map	map at: MpConstants raw16 put: #readRaw16.	map at: MpConstants raw32 put: #readRaw32.!

defineTrueActionTo: map	map at: MpConstants true put: #readTrue!

defineUnsignedIntegerActionTo: map	map at: MpConstants uint8 put: #readUint8.	map at: MpConstants uint16 put: #readUint16.	map at: MpConstants uint32 put: #readUint32.	map at: MpConstants uint64 put: #readUint64.!

on: bertDecoder 	^ self new decoder: bertDecoder;		 initActionMaps;		 yourself! !
!MpDecodeTypeMapper class categoriesFor: #defineArrayActionTo:!actions for compounds!public! !
!MpDecodeTypeMapper class categoriesFor: #defineCompoundsActionsTo:!actions for compounds!public! !
!MpDecodeTypeMapper class categoriesFor: #defineDoubleActionTo:!actions for primitives!public! !
!MpDecodeTypeMapper class categoriesFor: #defineFalseActionTo:!actions for primitives!public! !
!MpDecodeTypeMapper class categoriesFor: #defineFloatActionTo:!actions for primitives!public! !
!MpDecodeTypeMapper class categoriesFor: #defineIntegerActionTo:!actions for primitives!public! !
!MpDecodeTypeMapper class categoriesFor: #defineMapActionTo:!actions for compounds!public! !
!MpDecodeTypeMapper class categoriesFor: #defineNilActionTo:!actions for primitives!public! !
!MpDecodeTypeMapper class categoriesFor: #definePrimitivesActionsTo:!actions for primitives!public! !
!MpDecodeTypeMapper class categoriesFor: #defineRawBytesActionTo:!actions for primitives!public! !
!MpDecodeTypeMapper class categoriesFor: #defineTrueActionTo:!actions for primitives!public! !
!MpDecodeTypeMapper class categoriesFor: #defineUnsignedIntegerActionTo:!actions for primitives!public! !
!MpDecodeTypeMapper class categoriesFor: #on:!instance creation!public! !

MpEncodeTypeMapper guid: (GUID fromString: '{22BDA04F-F7A6-4B70-B71C-CC4FB07C836D}')!
MpEncodeTypeMapper comment: ''!
!MpEncodeTypeMapper categoriesForClass!MessagePack-Core! !
!MpEncodeTypeMapper methodsFor!

encoder	"Answer the value of encoder"	^ encoder!

encoder: anObject	"Set the value of encoder"	encoder := anObject!

writeObject: anObject ifNotApplied: aBlock 	| actionSelector |	actionMap		ifNotNil: [actionSelector := self actionMap at: anObject class ifAbsent: [].			actionSelector ifNotNil: [^ self encoder perform: actionSelector with: anObject]].			actionSelector := self defaultActionMap				at: anObject class				ifAbsent: [^ aBlock value].	^ self encoder perform: actionSelector with: anObject! !
!MpEncodeTypeMapper categoriesFor: #encoder!accessing!public! !
!MpEncodeTypeMapper categoriesFor: #encoder:!accessing!public! !
!MpEncodeTypeMapper categoriesFor: #writeObject:ifNotApplied:!actions!public! !

!MpEncodeTypeMapper class methodsFor!

defineArrayActionTo: map	map at: Array put: #writeArray:.!

defineCompoundsActionsTo: map	self defineArrayActionTo: map.	self defineMapActionTo: map.	!

defineDoubleActionTo: map	"Some dialect does not support Double"	"map at: Double put: #writeDouble:"!

defineFalseActionTo: map	map at: False put: #writeFalse:!

defineFloatActionTo: map	"Suppose 32 bit float - Some dialect does not support it"	"map at: Float put: #writeFloat:"!

defineIntegerActionTo: map	Integer allSubclasses do: [:each |		map at: each put: #writeInteger:	]!

defineMapActionTo: map		map at: Dictionary put: #writeMap:.	"map at: IdentityDictionary put: #writeDictionary:"		!

defineNilActionTo: map	map at: UndefinedObject put: #writeNil:!

definePrimitivesActionsTo: map	self defineRawBytesActionTo: map.	self defineIntegerActionTo: map.	self defineFloatActionTo: map.	self defineDoubleActionTo: map.	self defineNilActionTo: map.	self defineTrueActionTo: map.	self defineFalseActionTo: map.!

defineRawBytesActionTo: map	map at: ByteArray put: #writeRawBytes:!

defineTrueActionTo: map	map at: True put: #writeTrue:!

on: bertEncoder 	^ self new encoder: bertEncoder;		 initActionMaps;		 yourself! !
!MpEncodeTypeMapper class categoriesFor: #defineArrayActionTo:!actions for compounds!public! !
!MpEncodeTypeMapper class categoriesFor: #defineCompoundsActionsTo:!actions for compounds!public! !
!MpEncodeTypeMapper class categoriesFor: #defineDoubleActionTo:!actions for primitives!public! !
!MpEncodeTypeMapper class categoriesFor: #defineFalseActionTo:!actions for primitives!public! !
!MpEncodeTypeMapper class categoriesFor: #defineFloatActionTo:!actions for primitives!public! !
!MpEncodeTypeMapper class categoriesFor: #defineIntegerActionTo:!actions for primitives!public! !
!MpEncodeTypeMapper class categoriesFor: #defineMapActionTo:!actions for compounds!public! !
!MpEncodeTypeMapper class categoriesFor: #defineNilActionTo:!actions for primitives!public! !
!MpEncodeTypeMapper class categoriesFor: #definePrimitivesActionsTo:!actions for primitives!public! !
!MpEncodeTypeMapper class categoriesFor: #defineRawBytesActionTo:!actions for primitives!public! !
!MpEncodeTypeMapper class categoriesFor: #defineTrueActionTo:!actions for primitives!public! !
!MpEncodeTypeMapper class categoriesFor: #on:!instance creation!public! !

"Binary Globals"!

