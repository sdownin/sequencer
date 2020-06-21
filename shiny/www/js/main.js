/**
 *
 * SequenceR App
 *
 * main.js
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

function getNumPlots () {
    let nplots = 0;
	let measures = $('#analysis_measures_group input:checkbox:checked').map(function(){
      return $(this).val();
    }).get();
    if (measures.includes('distance')) {  nplots += 3 }
    if (measures.includes('predictability')) {  nplots += 1  }
    if (measures.includes('simplicity')) { 	nplots += 1 }
    if (measures.includes('grouping')) {  nplots += 1 }
    if (measures.includes('motif')) { nplots += 1  }
    console.log('nplots: ', nplots)
    return nplots;
}


$(document).ready(function(){

	// DISABLE NAV TABS ON LOAD BY REMOVING data-toggle="tab"
	// THEN ADD ATTRIBUTE BACK WHEN ADVANCED TO THAT STAGE OF NAVIGATION
	deactivateTab('Measures');
	deactivateTab('Outputs');
	deactivateTab('Plots');


	// NAVIGATION
	// 2. Measures
	$('button[id^=analysis_measures_goto]').click(function(){
		activateTab('Measures');
	});
	// 3. Outputs
	$('button[id^=analysis_run]').click(function(){
		activateTab('Outputs');
	});
	// 4. Plots
	$('button[id^=analysis_output_plots_button]').click(function(){
		activateTab('Plots');
	});


	// DEBUG FEATURES
	$('select[id^=analysis_distance_norm]').attr('disabled',true);

	// update plot region height
	$('#analysis_run').on('click', function(){

		$('.shiny-plot-output').hide();

		// console.log('measures clicked....setting plot area height');

		// let nplots = getNumPlots();

		// let $img = $('#analysis_output_plots img');

		// console.log('img:  ', $img);

	 //    $img.height( 290 * Math.ceil(nplots/3) );


    	let measures = $('#analysis_measures_group input:checkbox:checked').map(function(){
          return $(this).val();
        }).get();
    	if (measures.includes('distance')) { $('#analysis_output_plot_distance').show(); }
    	if (measures.includes('predictability')) { $('#analysis_output_plot_predictability').show(); }
    	if (measures.includes('motif')) { $('#analysis_output_plot_singles').show(); }
    	if (measures.includes('grouping')) { $('#analysis_output_plot_singles').show(); }
    	if (measures.includes('simplicity')) { $('#analysis_output_plot_singles').show(); }

	});

});

