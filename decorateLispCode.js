
//// 
//// ■DecorateLispCode#decorateLispCode()
//// Lisp コードを装飾する（HTMLタグで括る）
//// 　・（指定した）シンボル：<span class="operator"></span> で括る
//// 　・グローバル変数      ：<span class="variable"></span> で括る
//// 　・キーワード          ：<span class="keyword"></span> で括る
//// 　・文字列              ：<span class="string"></span> で括る
//// 　・コメント            ：<span class="comment"></span> で括る
//// 
//// ●必要なモジュール
//// 　・jQuery
//// 
//// ●既知の問題
//// 　・#| ～ |# 形式のコメントには未対応
//// 　・文字列中にセミコロンがある場合、同じ行のコメントは装飾されない
//// 
//// ●使い方
//// 　（例）
//// 　var decorate = new DecorateLispCode();
//// 　var lisOpe = ['car', 'cdr', 'atom', 'eq', 'cons'];	// 装飾を施す対象とするシンボル
////　
//// 　$('pre.code').each(function(i) {
//// 　    decorate.decorateLispCode(this, lisOpe);
//// 　});
//// 
//// 　jquery.inview.js と組み合わせて、Lisp コードが画面に表示されたタイミングで
//// 　装飾を施す方法もある。
//// 　（例）
//// 　<script type="text/javascript" src="jquery.inview.js"></script>
//// 　...
//// 　$('pre.code').one('inview', function(event, isInView, visiblePartX, visiblePartY) {
//// 　    decorate.decorateLispCode(this, lisOpe);
//// 　});
//// 


// クラス DecorateLispCode
var DecorateLispCode = function() {
};


// 指定DOM要素タグ以下の Lispコードを装飾する
// @param elm DOM要素
// @param lisOpe オペレータとして装飾するシンボルのリスト
DecorateLispCode.prototype.decorateLispCode = function(elm, lisOpe) {
	// シンボルで使える文字（全角文字に未対応）
	// '&amp;', '&lt;', '&gt;'は、&より前に配置しないと、
	// １文字ずつ適合されてしまう
	var reSymChar = '(?:&amp;|&lt;|&gt;|\\w|!|[\\$-%]|&(?!quot;)|\\*|\\+|[--/]|[<-@]|\\[|\\]|\\^|_|\\{|\\}|~)'; //>
	// シンボルの綴り（'|'によるエスケープに対応）
	var reSymWord = '(?:' + reSymChar + '+|\\|.*?\\|)';
	// シンボル　※数値も検出してしまうので注意
	var reSymbol = '(?:' + reSymWord + ':{1,2})?' + reSymWord;
	// キーワード
	var reKeyword = ':' + reSymWord;
	// コメント
	var reComment = ';.*$';
	// 文字列
	var reString = '("|&quot;)(?:.|\\n)*?("|&quot;)';

	// テキストとHTMLタグが別々になるよう抽出
	var txtTag = this.separateTagText(elm, true);
	var txt       = txtTag[0];
	var lisTagPos = txtTag[1];

	// 字句（シンボル、キーワード、文字列、コメント）の検出
	var lisSymbol  = this.searchRegexLabel('symbol', reSymbol, txt);
	var lisKeyword = this.searchRegexLabel('keyword', reKeyword, txt);
	var lisString  = this.searchRegexLabel('string', reString, txt);
	var lisComment = this.searchRegexLabel('comment', reComment, txt);

	// 上記検出結果の結合
	var lisPhrase = lisSymbol.concat(lisKeyword, lisString, lisComment);

	// 位置でソート
	lisPhrase = this.sortTagPosByPos(lisPhrase);

	// 範囲が重複している字句を削除
	lisPhrase = this.removeOverlapPhrase(lisPhrase);

	// 検出結果から、HTMLタグを追加
	this.addDecorateTagForLispPhrase(lisTagPos, lisPhrase, lisOpe);

	// テキストとHTMLタグ情報を統合
	var ht = this.unifyTagText(txt, lisTagPos);

	$(elm).html(ht);
};


// 指定要素内のテキストとHTMLタグの混在した文字列を、
// テキストとタグの情報（タグおよびその位置）に分離する
// @param elm
// @param cvBrFlg 真の場合、<br>前後に改行があるかチェックし、ない場合に改行を挿入
// @return 配列。
//		[0]:テキストのみを集めて結合した文字列
// 		[1]:タグの情報の配列。各要素の[0]はタグ、[1]は文字列内での位置。
DecorateLispCode.prototype.separateTagText = function(elm, cvBrFlg) {
	var ht = $(elm).html();

	var lisTagPos = this.parseHtmlTagPos(ht);

	var lisText = [];	// テキストのみ集めたリスト
	var accTagLen = 0;	// タグの長さの蓄積

	var start = 0;
	var end = 0;

	for (var idxTagPos = 0; idxTagPos < lisTagPos.length; idxTagPos++) {	// >
		var tagPos = lisTagPos[idxTagPos];

		end = tagPos[1];
		lisText.push(ht.substring(start, end));	// テキスト

		tagPos[1] -= accTagLen;			// 位置をずらす（書き替え）
		accTagLen += tagPos[0].length;	// タグの長さを加算

		start = end + tagPos[0].length;
	}

	end = ht.length;
	lisText.push(ht.substring(start, end));

	if (cvBrFlg) {
		// タグ<br>の前後に改行がない場合、改行を挿入する
		// また改行挿入により、タグの位置をずらす
		var regBr = new RegExp('^<br\\s*/?>');

		for (var idxTagPos = lisTagPos.length - 1; 0 <= idxTagPos; idxTagPos--) {	// >
			if (!lisTagPos[idxTagPos][0].match(regBr)) {	// タグが<br>か？
				continue;
			}
			if (this.tailEq(lisText[idxTagPos], '\n')) {	// <br>の直前が改行か？
				continue;
			}
			if (this.headEq(lisText[idxTagPos + 1], '\n')) {	// <br>の直後が改行か？
				continue;
			}
			// 改行を挿入し、タグの位置をずらす
			lisText[idxTagPos + 1] = '\n' + lisText[idxTagPos + 1];
			for (var idxTagPos2 = idxTagPos + 1; idxTagPos2 < lisTagPos.length; idxTagPos2++) {
				lisTagPos[idxTagPos2][1] += 1;
			}
		}
	}

	var result = [];
	result.push(lisText.join(''));
	result.push(lisTagPos);

	return result;
};


// separateHtmlText()で得られた情報（と同等の情報）から、
// テキストとHTMLタグ混在の文字列を作成する
// @param txt HTMLタグが埋め込まれる文字列
// @param lisTagPosOrg タグの情報の配列。各要素の[0]はタグ、[1]は文字列内での位置。
// @return 
DecorateLispCode.prototype.unifyTagText = function(txt, lisTagPosOrg) {
	// タグを位置でソート
	var lisTagPos = this.sortTagPosByPos(lisTagPosOrg.slice());

	// テキストとHTMLタグを混在させた結果文字列
	var result = '';

	for (var idxTagPos = lisTagPos.length - 1; 0 <= idxTagPos; idxTagPos--) {
		var tagPos = lisTagPos[idxTagPos];

		// タグより後ろにあるテキストを付加
		result = txt.substring(tagPos[1]) + result;
		txt = txt.substring(0, tagPos[1]);

		// タグを付加
		result = tagPos[0] + result;
	}
	result = txt + result;

	return result;
};


// 字句の情報を元に、HTMLタグ情報を追加
// @param lisTagPos HTMLタグ情報の追加先配列
// @param lisPhrase 字句情報の配列
// @param lisOpe 演算子として扱うシンボルの配列
DecorateLispCode.prototype.addDecorateTagForLispPhrase = function(lisTagPos, lisPhrase, lisOpe) {
	// 検出結果から、HTMLタグを追加
	for (var idxPhrase = 0; idxPhrase < lisPhrase.length; idxPhrase++) {	//>
		var phrase = lisPhrase[idxPhrase];

		var cls = false;

		if (phrase[2] == 'symbol') {
			if (2 <= phrase[0].length && this.headEq(phrase[0], '*') && this.tailEq(phrase[0], '*')) {
				cls = 'variable';
			} else if (lisOpe && 0 <= lisOpe.indexOf(phrase[0])) {		//>
				cls = 'operator';
			}
		} else if (phrase[2] == 'keyword' || phrase[2] == 'string' || phrase[2] == 'comment') {
			cls = phrase[2];
		}

		if (cls) {
			this.addEncloseTag(lisTagPos, phrase, cls);
		}
	}
};


// 指定の字句を囲む位置にHTMLタグ情報を追加
// @param lisTagPos HTMLタグの情報を加える先の配列
// @param phrase HTMLタグで囲む字句の情報（字句とその位置を示す配列。[0]:字句 [1]:位置）
// @param cls 追加するHTMLタグのclass値
DecorateLispCode.prototype.addEncloseTag = function(lisTagPos, phrase, cls) {
	tagStart = '<span class="' + cls + '">';
	tagEnd = '</span>';

	tagPosStart = [];
	tagPosStart.push(tagStart);
	tagPosStart.push(phrase[1]);

	tagPosEnd = [];
	tagPosEnd.push(tagEnd);
	tagPosEnd.push(phrase[1] + phrase[0].length);

	lisTagPos.push(tagPosStart);
	lisTagPos.push(tagPosEnd);
};


// 範囲が重複している字句を削除
// @param lisPhraseOrg 字句情報のリスト
// @return 範囲重複している字句が削除されたリスト
DecorateLispCode.prototype.removeOverlapPhrase = function(lisPhraseOrg) {
	var lisPhrase = lisPhraseOrg.slice();	// 複製

	for (var idxPhrase = 0; idxPhrase < lisPhrase.length - 1; idxPhrase++) {	//>
		var tagPos = lisPhrase[idxPhrase];
		var tagEnd = tagPos[1] + tagPos[0].length;

		// 上記字句の末尾より前から始まっている字句は削除
		while (idxPhrase < lisPhrase.length - 1) {	//>
			if (lisPhrase[idxPhrase + 1][1] < tagEnd) {	//>
				lisPhrase.splice(idxPhrase + 1, 1);	// 削除
				continue;
			}
			break;
		}
	}

	return lisPhrase;
};


// タグと位置の配列を要素とする配列を、位置でソートする
// @param lisTagPosOrg ソートする配列。
// 		各要素は、[0]:HTMLタグ　[1]:タグの位置
// @return 
DecorateLispCode.prototype.sortTagPosByPos = function(lisTagPosOrg) {
	var lisTagPos = lisTagPosOrg.slice();	// 複製

	// 安定したソートにする為、各要素にプロパティを付加
	for (var idx = 0; idx < lisTagPos.length; idx++) {
		lisTagPos[idx].__idxOrg__ = idx;
	}

	lisTagPos.sort(function(tagPos1, tagPos2) {
		if (tagPos1[1] < tagPos2[1]) {
			return -1;
		} else if (tagPos2[1] < tagPos1[1]) {
			return 1;
		}

		return (tagPos1.__idxOrg__ - tagPos2.__idxOrg__);
	});

	// 安定したソートにする為に付加したプロパティを削除
	for (var idx = 0; idx < lisTagPos.length; idx++) {
		delete lisTagPos[idx].__idxOrg__;
	}

	return lisTagPos;
};


// 先頭の要素が第２要素に等しいか
// @param seq （要素数が0でもエラーにならない）
// @param ch
// @return 
DecorateLispCode.prototype.headEq = function(seq, ch) {
	return (seq.length >= 1 && seq[0] == ch);
};


// 末尾の要素が第２要素に等しいか
// @param seq （要素数が0でもエラーにならない）
// @param ch
// @return 
DecorateLispCode.prototype.tailEq = function(seq, ch) {
	return (seq.length >= 1 && seq[seq.length - 1] == ch);
};


// 文字列に含まれるHTMLタグとその位置を返す
// @param ht 文字列
// @return HTMLタグとその位置を要素とする配列。
// 		各要素は、[0]:HTMLタグ　[1]:タグの位置
DecorateLispCode.prototype.parseHtmlTagPos = function(ht) {
	var reTagName  = '[^ \f\n\r\t\v\>\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]+';
	var rePropName = '[^ \f\n\r\t\v\=\>\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]+';
	var rePropVal1 = '[^ \f\n\r\t\v\>\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]*';
	var rePropVal2 = '"[^"]*"';
	var rePropVal3 = "'[^']*'";

	var reProp = '\\s*' + rePropName + '(?:=(?:' + rePropVal1 + '|' + rePropVal2 + '|' + rePropVal3 + '))?';

	var reTag = '</?' + reTagName + '(?:' + reProp + ')*\\s*/?>';
	var lisTagPos = this.searchRegex(reTag, ht);

	return lisTagPos;
};


// 正規表現で検索後、結果の要素（配列）にラベルを追加
// @param label ラベル（任意）
// @param reStr 正規表現（文字列）
// @param str 検索対象の文字列
// @param flags 省略時'gm'
// @return 
DecorateLispCode.prototype.searchRegexLabel = function(label, reStr, str, flags) {
	if (!flags) {
		flags = 'gm';
	}
	var lis = this.searchRegex(reStr, str, flags);

	lis.forEach(function(elm) {
		elm[2] = label;
	});

	return lis;
};


// 正規表現で見つかった文字列とその位置を求める
// @param reStr 正規表現を示した文字列
// @param str
// @param flags 検索に用いるフラグ。'g'は内部で必ず指定される。
// @return 見つかった文字列とその位置を要素とする配列。
// 		各要素は、[0]:見つかった文字列　[1]:見つかった文字列の位置
DecorateLispCode.prototype.searchRegex = function(reStr, str, flags) {
	if (!flags) {
		flags = 'g';
	} else if (flags.indexOf('g') < 0) {
		flags = 'g' + flags;
	}

	var reg = new RegExp(reStr, flags);

	// 各要素は配列。
	// 各要素の[0]:見つかった文字列 [1]:見つかった文字列の位置
	var lisMatchPos = [];

	// HTMLタグの位置を調べる
	var rreg;
	while (rreg = reg.exec(str)) {
		var matchPos = [];
		matchPos[0] = rreg[0];
		matchPos[1] = rreg.index;
		lisMatchPos.push(matchPos);
	}

	return lisMatchPos;
};

