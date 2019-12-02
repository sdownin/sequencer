/**
 *
 * SequenceR App
 *
 * script.js
 *
 */

$(document).ready(function(){

	$('button[id^=analysis_measures_show]').click(function(){
		$('.nav-tabs a[data-value=Measures]').tab('show');
	});

	$('button[id^=analysis_run]').click(function(){
		$('.nav-tabs a[data-value=Outputs]').tab('show');
	});

});