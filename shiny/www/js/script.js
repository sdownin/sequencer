/**
 *
 * SequenceR App
 *
 * script.js
 *
 */

// Deactivate navtab
function deactivateTab(dataVal) {
	$('.nav-tabs  a[data-value='+dataVal+']')
		.removeAttr('data-toggle')
		.parent('li')
			.addClass('disabled');
}

// Activate navtab
function activateTab(dataVal) {
	$('.nav-tabs a[data-value='+dataVal+']')
		.attr('data-toggle','tab')
		.tab('show')
		.parent('li')
			.removeClass('disabled');
}


$(document).ready(function(){

	// DISABLE NAV TABS ON LOAD BY REMOVING data-toggle="tab"
	// THEN ADD ATTRIBUTE BACK WHEN ADVANCED TO THAT STAGE OF NAVIGATION
	deactivateTab('Measures');
	deactivateTab('Outputs');
	deactivateTab('Plots');


	// NAVIGATION
	$('button[id^=analysis_measures_goto]').click(function(){
		activateTab('Measures');
	});

	$('button[id^=analysis_plots_goto]').click(function(){
		activateTab('Plots');
	});

	$('button[id^=analysis_run]').click(function(){
		activateTab('Outputs');
	});

	// DEBUG FEATURES
	$('select[id^=analysis_distance_norm]').attr('disabled',true);

});