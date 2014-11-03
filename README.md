# GR-PARSEVAL - different variants of Parseval constituency parser evaluation metric

## Quickstart

Step 1. If you don't have Lisp installed, do that. We recommend [SBCL](http://www.sbcl.org/)
or [Clozure CL](http://ccl.clozure.com/).

Step 2. Get [quicklisp](http://www.quicklisp.org/beta/) and set it up.
It's simple, follow the instructions.

Step 3. Download the source code to `~/quicklisp/local-projects/gr-parseval/`

Step 4. Start your Lisp. After it loads, in the prompt type: `(ql:quickload :gr-parseval)`

Step 5. If everything loaded fine and without any errors, you can experiment now. Here's how it may look:

    CL-USER> (parseval:parseval '(NP (NNS "foo") (CC "and") (NN "bar"))
                                '(NP (NP (NN "foo")) (CC "and") (NN "bar")))
    (3/5 3/4 2/3)
    (3 4 5)

This means that you have precision of 3/5, recall of 3/4 and F1 of 2/3.
There are 4 nodes in the test tree (comes first), 5 nodes in the gold tree,
and 3 nodes have matched.

Step 6. You need to supply trees in the format that Lisp will understand.
Basically, it means that word-level nodes should be quoted,
and some special characters should be escaped.

If you have some existing corpus you'd like to experiment with,
you'll need to read and adapt the trees from it. If the corpus is in Penn Treebank format,
you can do it with the utilities from [cl-nlp](https://github.com/vseloved/cl-nlp).
Here's a [more elaborate description](http://lisp-univ-etc.blogspot.com/2014/09/how-to-write-english-pos-tagger-with-cl.html) - see section "Available data and tools to process it".


## Contributors

- Vsevolod Dyomkin <vseloved@gmail.com>
- Mariana Romanyshyn <mariana.scorp@gmail.com>
- Maxim Zholobak <ghostcheka@gmail.com>


## License

 Copyright (c) 2014 Grammarly Inc.

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 Except as contained in this notice, the name(s) of the above
 copyright holders shall not be used in advertising or otherwise
 to promote the sale, use or other dealings in this Software
 without prior written authorization.

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.
