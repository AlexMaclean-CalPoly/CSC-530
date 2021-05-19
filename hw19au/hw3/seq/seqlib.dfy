/** 
 * Part I: Specify and implement sequence reversal, sequence slice reversal, and sequence rotation. 
 **/

/**
 * sreverse takes as input a sequence s of elements of type T, 
 * and it returns a new sequence that reverses the order of elements in s.
 * For example, sreverse([1,2,3]) should return [3,2,1].
 * Add enough annotations (~2) to sreverse to enable Dafny to prove the 
 * reverseLemma and the assertions in the sreverseClient. 
 * You may not modify reverseLemma or sreverseClient in any way.
 */
function method sreverse<T>(s : seq<T>) : seq<T>
{
    if (|s| == 0) then s else sreverse(s[1..]) + [s[0]]
}

/** 
 * The sreverseClient exercises the sreverse definition by asserting 
 * some simple properties. Do not change the client code. The assertions 
 * will pass once sreverse is correctly annotated.
 */
 method sreverseClient() {
     assert sreverse<int>([]) == [];
     assert sreverse([1]) == [1];
     assert sreverse([1,2]) == [2,1];
     assert sreverse([1,2,3]) == [3,2,1];
 }

/** 
 * The reverseLemma says that reversing a sequence twice results 
 * in an equal sequence (i.e., same elements, in same order).
 * Do not change the lemma. Dafny will be able to prove it once 
 * sreverse is correctly annotated. 
 */
lemma reverseLemma<T>(s : seq<T>)
ensures s == sreverse(sreverse(s));
{}

/**
 * ssubreverse takes as input a sequence s, a start index, and an end index, 
 * and returns a new sequence that is like s except that the subsequence between 
 * the start index, inclusive, and the end index, exclusive, is reversed. 
 * For example, ssubreverse([1,2,3,4], 1, 3) == [1,3,2,4]. 
 * Implement ssubreverse (1 line of code) and write enough annotatations (~1) 
 * to help Dafny prove subreverseLemma (which will also need ~1 annotation) 
 * and the assertions in ssubreverseClient. You may not modify ssubreverseClient 
 * in any way, but you may add more annotations to subreverseLemma.
 */
function method ssubreverse<T>(s : seq<T>, start : int, end : int) : seq<T>
{
    [] // <<- insert correct implementation here
}

/** 
 * The ssubreverseClient exercises the ssubreverse definition by asserting 
 * some simple properties. Do not change the client code. The assertions 
 * will pass once ssubreverse is correctly implemented and annotated.
 */
 method ssubreverseClient() {
     assert ssubreverse<int>([],0,0) == [];
     assert ssubreverse([1],0,0) == [1];
     assert ssubreverse([1],0,1) == [1];
     assert ssubreverse([1],1,1) == [1];
     assert ssubreverse([1,2],0,1) == [1,2];
     assert ssubreverse([1,2],1,1) == [1,2];
     assert ssubreverse([1,2],1,2) == [1,2];
     assert ssubreverse([1,2],2,2) == [1,2];
     assert ssubreverse([1,2],0,2) == [2,1];
     assert ssubreverse([1,2,3],0,2) == [2,1,3];
     assert ssubreverse([1,2,3,4],0,4) == [4,3,2,1];
     assert ssubreverse([1,2,3,4],1,3) == [1,3,2,4];
     assert ssubreverse([1,2,3,4],1,4) == [1,4,3,2];
     assert ssubreverse([1,2,3,4],0,2) == [2,1,3,4];
 }

/** 
 * The subreverseLemma says that applying ssubreverse twice to 
 * a sequence with the same start and end results in an equal 
 * sequence (i.e., same elements, in same order).
 * You may add more annotations (~1) to help Dafny prove this lemma. 
 */
lemma subreverseLemma<T>(s : seq<T>, start : int, end : int)
ensures s == ssubreverse(ssubreverse(s, start, end), start, end)
{}

/**
 * srotate takes as input a sequence s and a size k, 
 * and returns a new sequence that concatenates the last k 
 * elements of s with the first |s|-k elements of s.  
 * For example, srotate([1,2,3,4], 2) == [3,4,1,2]. 
 * Implement srotate (1 line of code) and write enough annotatations (~1) 
 * to help Dafny prove rotateLemma (which will also need ~1 annotation) 
 * and the assertions in srotateClient. You may not modify srotateClient 
 * in any way, but you may add more annotations to srotateLemma.
 */
function method srotate<T>(s: seq<T>, k: int) : seq<T>
{
    [] // <<- insert correct implementation here
}

/** 
 * The srotateClient exercises the srotate definition by asserting 
 * some simple properties. Do not change the client code. The assertions 
 * will pass once srotate is correctly implemented and annotated.
 */
 method srotateClient() {
    assert srotate<int>([],0) == [];
    assert srotate([1],0) == [1];
    assert srotate([1],1) == [1];
    assert srotate([1,2],0) == [1,2];
    assert srotate([1,2],1) == [2,1];
    assert srotate([1,2,3],0) == [1,2,3];
    assert srotate([1,2,3],1) == [3,1,2];
    assert srotate([1,2,3],2) == [2,3,1];
    assert srotate([1,2,3,4], 2) == [3,4,1,2];
 }

/** 
 * The rotateLemma says that applying srotate twice to 
 * a sequence with suitable argument k results in an equal 
 * sequence (i.e., same elements, in same order).
 * You may add more annotations (~1) to help Dafny prove this lemma. 
 */
lemma rotateLemma<T>(s: seq<T>, k: int)
ensures s == srotate(srotate(s, k), |s| - k)
{ }

/** 
 * Part II: Specify and implement in-place array reversal, array slice reversal, and array rotation. 
 **/

/**
 * The methods subreverse and swap are fully implemented: subreverse uses swap to implement 
 * in-place reversal of a subarray in a given array. In other words, subreverse is an 
 * efficient imperative implementation of ssubreverse. Add enough annotations to swap (~6) and 
 * subreverse (~7) so that Dafny can verify both of them, as well as as the subreverseClient. 
 * You may not change the code of either these methods or subreverseClient
 */

/** Swaps the elements at the given indices in the input array. ~6 annotations needed. **/
method swap<T>(v : array<T>, i: int, j: int) 
{
    var tmp := v[i];
    v[i] := v[j];
    v[j] := tmp;
}

/**
 * Takes as input an array v and start and end indices, and 
 * mutates v to reverse the elements from start, inclusive, to 
 * end, exclusive, in linear time and constant space.
 * ~7 annotations needed.
 */
method subreverse<T>(v : array<T>, start: int, end: int)
ensures v[..] == ssubreverse(old(v[..]), start, end)
{
    var left := start;
    var right := end - 1;
    while (left < right) 
    {
        swap(v, left, right);
        left := left + 1;
        right := right - 1;
    }
}

/** 
 * The subreverseClient exercises the subreverse method by asserting 
 * some simple properties. Do not change the client code. The assertions 
 * will pass once subreverse is correctly annotated.
 */
 method subreverseClient() {
     var a0 := new int[0];
     subreverse(a0,0,0);
     assert a0.Length == 0;
     var a1 := new int[1];
     a1[0] := 1;
     subreverse(a1,0,0);
     assert a1[0] == 1;
     subreverse(a1,0,1);
     assert a1[0] == 1;
     subreverse(a1,1,1);
     assert a1[0] == 1;
     var a4 := new int[3];
     a4[0] := 1; a4[1] := 2; a4[2] := 3;  
     subreverse(a4,1,3);
     assert a4[0] == 1 && a4[1] == 3 && a4[2] == 2;
 }
 
/**
 * reverse implements in-place, linear time reversal of the input array v.
 * Complete the implementation of reverse (~1 line) and specification (~1 line) of 
 * reverse to enable Dafny to verify both reverse and reverseClient. 
 */
method reverse<T>(v : array<T>)
ensures v[..] == sreverse(old(v[..])) 
{
}

/** 
 * The reverseClient exercises the reverse method by asserting 
 * some simple properties. Do not change the client code. The assertions 
 * will pass once reverse is correctly implemented and annotated.
 */
 method reverseClient() {
     var a0 := new int[0];
     reverse(a0);
     assert a0.Length==0;
     var a1 := new int[1];
     a1[0] := 1;
     reverse(a1);
     assert a1[0] == 1;
     var a2 := new int[2];
     a2[0] := 1; a2[1] := 2;
     reverse(a2);
     assert a2[0] == 2 && a2[1] == 1;
 }

/**
 * rotate implements in-place, linear time rotation of the input array v.
 * Complete the implementation of rotate (~4 lines) and specification (~2 lines) of 
 * rotate to enable Dafny to verify both rotate and rotateClient.
 */
method rotate<T>(v: array<T>, k: int) 
ensures v[..] == srotate(old(v[..]), k)
{
}

/** 
 * The rotateClient exercises the rotate method by asserting 
 * some simple properties. Do not change the client code. The assertions 
 * will pass once rotate is correctly implemented and annotated.
 */
 method rotateClient() {
    var a0 := new int[0];
    rotate(a0,0);
    assert a0.Length==0;
    var a1 := new int[1];
    a1[0] := 1;
    rotate(a1,0);
    assert a1[0] == 1;
    rotate(a1,1);
    assert a1[0] == 1;
    var a2 := new int[2];
    a2[0] := 1; a2[1] := 2;
    rotate(a2, 1);
    assert a2[0] == 2 && a2[1] == 1;
 }