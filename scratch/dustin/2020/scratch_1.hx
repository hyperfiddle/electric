package
// FLOW
// flow is the definition of a self-adjusting computation
// like a riverbed without water and we're not sure which branches are wet or dry yet
// this is the maximum spanning graph of anything that could happen.
// "what happens" is controlled by continuation functions, so you can't actually
// know how the river will evaluate in advance of actually evaluating it,
// without understanding of the AST defining the flow and the closures encoded into it.
//
// DAG
// dag is the flow at some time T. It's the water in the river today.
// If we know what time it is, we know how the continuations have unfolded so far.
// So we can see which parts of the river are dry or wet.
//
// ACTIVATION
// These are push streams. Inputs push forward eagerly and thus are suitable for running
// effects. Also, we have static signal nodes, which are sampled. These must be constants
// or our algorithm breaks.
//
// Inputs only push if someone is listening, thus they have an activation state.
// When someone is listening, we pull the first value through. From that point onward,
// the inputs will push new values forward.
//
// what is difference between activate and attach, deactivate and detach?
//   detach doesn't necessarily deactivate, if remaining subscribers
//   attach doesn't necessarily activate,   if already activated
//
// RANK
// When values push through a DAG, we must plan the order in which the DAG's nodes run
// so that each node runs exactly once. This is a topological ordering of the DAG.
// As new nodes are attached to the graph, the topo ordering must be recomputed.
//
// rank is about inputs/outputs, and control nodes that can adjust the inputs/outputs
// rerank when the inputs/outputs/control node changes

// Incremental
//
// Say that a node is "observed" if there is an observer for it (created via [observe]).
// Say that a node is "necessary" if there is a path from that node to an observed node.
// [stabilize] ensures that all necessary nodes have correct values; it will not compute
// unnecessary nodes.  An unobserved node becomes necessary by a call to [observe] or by
// being used to compute an observed node; this will cause the appropriate DAG edges to
// be added.  A necessary node will become unnecessary if its observer (if any) becomes
// unused and if the node is no longer used to compute any observed nodes.  This will
// cause the appropriate DAG edges to be removed.
//
// [stabilize] traverses the DAG in topological order starting at variables that changed
// since the last stabilization and recomputing their dependents.  This is done by using
// a "recompute heap" to visit the nodes in non-decreasing order of "height", which is a
// over-approximation of the longest path from a variable to that node.  To ensure that
// each node is computed at most once and that its children are stabilized before it is
// computed, nodes satisfy the property that if there is an edge from n1 to n2, then the
// height of n1 is less than the height of n2.




// First, we set up the flow
// Then, we activate an output
// Activation propogates via "ups", attaching on the way up
//   Nodes are sampled, then planned
//   effects fire at output nodes in this frame if the inputs were ready
//
// inputs were not ready
// > control :q
//   cross runs
//   q activates/attaches
// > q 2
//   z fires


// When do we pump?
// Pump re-entrancy?
// Pump locking?
// Redo the ranking in the middle of a pump?











class X {

    function rerank (b : Push<Dynamic>) {
        // everything downstream of a topo change needs rerank

        // go back to all sources
        // find all sinks
        // from the sinks, go to sources, then traverse
    }

    function push3 (a : Push<Dynamic>) {
        // if inputs, go higher
        // but ... do this in order???
        // can't we just like, run
        switch (a.def) {
            case Const(v): {
                // if we're in an on, flow the first time?
                // if we're in a push, don't interrupt?
                /*if (!firstTime)*/ resume(a, Val(v)); // the lock does the right thing here
            }
            default:
        }
    }

    // activate computes a's rank (input ranks)
    // rerank b based on new a ? (once all inputs done)
    //   rerank bc the inputs just changed due to this attach
    //   rerank b needs to rerank c, d ... z
    // after any set of attaches are done, need to rerank downtree (uptree does not need change)

    function computeRank(b : Push<Dynamic>){
        var deps = b.inputs.concat(b.ups);
        b.rank = [for(a in deps) a.rank].fold(cast Math.max, b.rank);
        // inputs as well as ups !
        if(b.joins()) b.rank++;
        //for(c in b.outputs) computeRank(c);
    }
}

    // When computing rank because something attached due to either activation
    // or bind, this should only impact downtree ranks (all of them).
    // ranks uptree from me are good
    // my rank is now X
    // rank downtree from me needs to be at least my rank (reconsider my new rank)
    //
    // now that ranks are calculated,
    // we could push here, if the lock was a counter
    //
    // probably need splice-ranking to get this right


// Activation process:
// 1. Set the in/out links, fire lifecycle here.  (traversal 1)
// 2. Recalculate ranks from the in/out links (only downstream of attach) (traversal 2)
// 3. Push newly attached Const nodes. In a transaction?

// Bind causes new attach/detach, process:
// 1. set the in/out links that have just changed, and activate process if new. Fire lifecycle here
// 2. Recalculate ranks from new in/out links (only downstream of the new attach has changed)
// 3. Push newly attached Const nodes
