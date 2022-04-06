$( document ).ready(function() {
 
 var word_provizioane_specifice = $('#home_ui_1-word_provizioane_specifice');
 
 var upload_plati = $('#home_ui_1-upload_fisier_plati');
 
 var baza_date_provizioane_plati = $('#home_ui_1-baza_provizioane_plati');
 
 var coeficienti_plati = $('#home_ui_1-coeficienti_plati');
 
 var utils_provizioane_depreciate = $('#home_ui_1-upload_utile_solduri');
 
 var upload_fisier_solduri = $('#home_ui_1-upload_fisier_solduri');
 
 var baza_date_solduri = $('#home_ui_1-baza_provizioane_solduri');
 
var box_utility_sold =  document.querySelector("#database_portofoliu_ui_1-box_utility > div.card-header > div > button:nth-child(1) > i")

var box_upload_sold = document.querySelector("#database_portofoliu_ui_1-box_upload_portofoliu > div.card-header > div > button:nth-child(1) > i")

var box_database_sold = document.querySelector("#database_portofoliu_ui_1-box_plati_database > div.card-header > div > button:nth-child(1) > i")

var box_coeficienti_sold = document.querySelector("#database_portofoliu_ui_1-box_coeficienti_portofoliu > div.card-header > div > button:nth-child(1) > i")

 var box_word_plati = document.querySelector("#provizioane_plati_ui_1-box_word_plati > div.card-header > div > button:nth-child(1) > i")
 
 var box_upload_plati = document.querySelector("#provizioane_plati_ui_1-box_upload_plati > div.card-header > div > button:nth-child(1) > i")
 
 var box_database_plati = document.querySelector("#provizioane_plati_ui_1-box_database_plati > div.card-header > div > button:nth-child(1) > i")
 
 var box_coeficient_plati =  document.querySelector("#provizioane_plati_ui_1-box_coeficienti_plati > div.card-header > div > button:nth-child(1) > i")
 
 utils_provizioane_depreciate.on( 'click', function() {
 $('#tab-solduri').click();
 if ( box_utility_sold.classList.contains("fa-plus") ) { box_utility_sold.click() };
 if ( box_upload_sold.classList.contains("fa-minus") ) { box_upload_sold.click() };
 if ( box_database_sold.classList.contains("fa-minus") ) { box_database_sold.click() };
 if ( box_coeficienti_sold.classList.contains("fa-minus") ) { box_coeficienti_sold.click() };
 
});

upload_fisier_solduri.on( 'click', function() {
 $('#tab-solduri').click();
 if ( box_utility_sold.classList.contains("fa-minus") ) { box_utility_sold.click() };
 if ( box_upload_sold.classList.contains("fa-plus") ) { box_upload_sold.click() };
 if ( box_database_sold.classList.contains("fa-minus") ) { box_database_sold.click() };
 if ( box_coeficienti_sold.classList.contains("fa-minus") ) { box_coeficienti_sold.click() };
 
});

baza_date_solduri.on( 'click', function() {
 $('#tab-solduri').click();
 if ( box_utility_sold.classList.contains("fa-minus") ) { box_utility_sold.click() };
 if ( box_upload_sold.classList.contains("fa-minus") ) { box_upload_sold.click() };
 if ( box_database_sold.classList.contains("fa-plus") ) { box_database_sold.click() };
 if ( box_coeficienti_sold.classList.contains("fa-minus") ) { box_coeficienti_sold.click() };
 
});
 
 baza_date_provizioane_plati.on( 'click', function() {
 $('#tab-plati').click();
 if ( box_word_plati.classList.contains("fa-minus") ) { box_word_plati.click() };
 if ( box_database_plati.classList.contains("fa-plus") ) { box_database_plati.click() };
 if ( box_upload_plati.classList.contains("fa-minus") ) { box_upload_plati.click() };
 if ( box_coeficient_plati.classList.contains("fa-minus") ) { box_coeficient_plati.click() }
});
 
 coeficienti_plati.on( 'click', function() {
 $('#tab-plati').click();
 if (box_database_plati.classList.contains("fa-minus") ) { box_database_plati.click() };
 if (box_upload_plati.classList.contains("fa-minus") ) { box_upload_plati.click() };
 if (box_coeficient_plati.classList.contains("fa-plus") ) { box_coeficient_plati.click() };
if ( box_word_plati.classList.contains("fa-minus") ) { box_word_plati.click() };
 
});

upload_plati.on( 'click', function() {
 $('#tab-plati').click();
 if (box_database_plati.classList.contains("fa-minus") ) { box_database_plati.click() };
 if (box_upload_plati.classList.contains("fa-plus") ) { box_upload_plati.click() };
 if (box_coeficient_plati.classList.contains("fa-minus") ) { box_coeficient_plati.click() }
  if ( box_word_plati.classList.contains("fa-minus") ) { box_word_plati.click() };
});

word_provizioane_specifice.on( 'click', function() {
 $('#tab-plati').click();
 if ( box_word_plati.classList.contains("fa-plus") ) { box_word_plati.click() };
 if ( box_database_plati.classList.contains("fa-minus") ) { box_database_plati.click() };
 if ( box_upload_plati.classList.contains("fa-minus") ) { box_upload_plati.click() };
 if ( box_coeficient_plati.classList.contains("fa-minus") ) { box_coeficient_plati.click() }
});




});
