# Portable CommonLoops (for modern CL platforms)
A portable CLOS/MOP package for modern CL platforms

## Project Goals
- Create a ASDF-compliant PCL package which is loadable in all major modern CL platforms.
- Help new CL implementations quickly getting ready-to-use CLOS implementations
- Help existing CL implementations (those still use Closette, i.e. ABCL, Corman, XCL) upgrading to PCL easier,
- Synchronize PCL patches between different CL implementations (i.e. CMUCL, SBCL),
- Incorporate new MOP features into PCL, from other non-Closette non-PCL implementations (i.e. Clozure CL, CLISP),
- Minimize the support efforts for "Closer to MOP" project when this project is used by any CL implementation.

## What did we begin with
- The original Closette 1.0 from the book AMOP,
- The latest Portable CommonLoops from Xerox, version "September 16 92 PCL (f)"

## Backgrounds
- Closette 1.0 is easy to adopt, but it's not ANSI-compliant (i.e. lack of BUILT-IN-CLASS). Can we fix these issues in a portable way without increasing the complexity of Closette? (Closette code has been moved to a separated project)
- Gerd Moellmann has been working on improving CMU CL's PCL-based CLOS implementation. He added new features and tuned performance, including an experimental optimization that makes class slot access as fast as structure slot access. Can SBCL and other PCL-based CLs benefit from these work? (I confirm that SB-PCL doesn't have Gerd's work)
- The newly open-sourced CormanLisp need a better CLOS, PCL is a good candicate, but how to keep the good performance in old CLOS which is optimized manually by Roger Corman using x86 assembly code? (i.e. can we bootstrap PCL from Closette and use both of them?)
- ABCL needs a better CLOS too.

## What do we have now
- Nothing on Jun 3, 2015
- Initial analysis on PCL's code base on Oct 25, 2016
- Re-organization according to GCL's code base on Oct 25, 2016
- Gerd's PCL optimization & regression test code added from CMUCL code base.

## Maintainers
- Chun Tian (binghe) <binghe.lisp@gmail.com>
