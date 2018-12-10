/*
WebWorkerに仕事を依頼するのは、
フロントが遅くなると困るからなだけ。

プレイヤー数と石の数の上限を引数に起動され、
全プレイヤーのスコアと盤面の最終状態を結果として返す。

オブジェクトの交換できなかった気が。
*/

function DList(v) {
  this.val = v;
  this.pred = null;
  this.succ = null;
}

this.onmessage = function (event) {
  var n = event.data.n;
  var bmax = event.data.bmax;

  var stage = new DList(0);
  stage.pred = stage;
  stage.succ = stage;

  var scores = new Array(n);
  for (var i = 0; i < n; i++) { scores[i] = 0; }

  for (var i = 1; i <= bmax; i++) {
    if (i % 23 == 0) {
	/* backword */
	stage = stage.pred.pred.pred.pred.pred.pred;
	var b2 = stage.pred;
	var b1 = b2.pred;
	scores[i % n] += i + b2.val;
	b1.succ = stage; stage.pred = b1;
    } else {
	/* forward */
	var b1 = stage.succ;
	var b2 = b1.succ;
	stage = new DList(i);
	b1.succ = stage; stage.pred = b1;
	b2.pred = stage; stage.succ = b2;
    }
  }

//  var result = -1;
//  for (var i = 0; i < n; i++) { result = Math.max(result, scores[i]); }
//  postMessage({maxscore : result});

  postMessage({maxscore : Math.max.apply(undefined,scores)});
}
