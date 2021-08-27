/*  Below are all edits to be made in src/changestats.c in the ergm 
 *  source code. These binary terms are needed for the valued ERGM terms
 *  to operate.
 */


/*****************                       
 changestat: d_nodematchhigh
*****************/
D_CHANGESTAT_FN(d_nodematchhigh) { 
  double matchval;
  Vertex tail, head;
  int i, edgeflag, cut, nrow, nnodes;

  /* *** tail -> head */    
  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    tail=TAIL(i);
    head=HEAD(i);
    matchval = INPUT_PARAM[tail];
    if (matchval == INPUT_PARAM[head]){ /* We have a match! */
      edgeflag = IS_OUTEDGE(tail, head);
      CHANGE_STAT[0] += edgeflag ? -1.0 : 1.0;
    }
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}

/*****************                       
 changestat: d_nodematchlow
*****************/
D_CHANGESTAT_FN(d_nodematchlow) { 
  double matchval;
  Vertex tail, head;
  int i, edgeflag, cut, nrow, nnodes;

  /* *** tail -> head */    
  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    tail=TAIL(i);
    head=HEAD(i);
    matchval = INPUT_PARAM[tail];
    if (matchval == INPUT_PARAM[head]){ /* We have a match! */
      edgeflag = IS_OUTEDGE(tail, head);
      CHANGE_STAT[0] += edgeflag ? -1.0 : 1.0;
    }
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}

/*  Below are all edits to be made in src/wtchangestats.c in the ergm 
 *  source code. These edits are needed to interact with the valued ERGM 
 *  terms directly.
 */
 
 
 /*****************
 changestat: d_nodematchhigh_nonzero
*****************/
WtD_CHANGESTAT_FN(d_nodematchhigh_nonzero) { 
int cut = INPUT_PARAM[0];



  /* *** tail -> head */    
  EXEC_THROUGH_TOGGLES({
      double matchval = INPUT_PARAM[TAIL];
      if (matchval == INPUT_PARAM[HEAD])  { /* We have a match! */
	double s = (NEWWT >= cut) - (OLDWT >= cut);
	CHANGE_STAT[0] += s;
	}
    });
}

/*****************
 changestat: d_nodematchlow_nonzero
*****************/
WtD_CHANGESTAT_FN(d_nodematchlow_nonzero) { 
int cut = INPUT_PARAM[0];



  /* *** tail -> head */    
  EXEC_THROUGH_TOGGLES({
      double matchval = INPUT_PARAM[TAIL];
      if (matchval == INPUT_PARAM[HEAD])  { /* We have a match! */
	double s = (NEWWT < cut && NEWWT > 0) - (OLDWT < cut && OLDWT > 0);
	CHANGE_STAT[0] += s;
	}
    });
}