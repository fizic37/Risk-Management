function cip_delete_module_js(ns_prefix) {
  
  $("#" + ns_prefix + "cip_database").on("click", ".delete_btn", function() {
    Shiny.setInputValue(ns_prefix + "id_data_raport_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  }); 
  
  $("#" + ns_prefix + "cip_database").on("click", ".download_btn", function() {
    Shiny.setInputValue(ns_prefix + "id_data_raport_to_download", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
}