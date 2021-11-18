function crc_module_js(ns_prefix) {
  
  $("#" + ns_prefix + "sumar_baza_date_crc").on("click", ".delete_btn", function() {
    Shiny.setInputValue(ns_prefix + "data_raport_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  }); 
  
  $("#" + ns_prefix + "sumar_baza_date_crc").on("click", ".download_btn", function() {
    Shiny.setInputValue(ns_prefix + "data_raport_to_download", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
}