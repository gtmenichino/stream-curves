(function() {
  if (typeof Shiny === "undefined" || typeof $ === "undefined") {
    return;
  }

  function closeSummaryPickers(rowId) {
    var $scope = rowId ? $("#" + rowId) : $(document);

    $scope.find(".summary-picker-cell .bootstrap-select.show > .dropdown-toggle").each(function() {
      $(this).trigger("click");
    });

    window.setTimeout(function() {
      $("body > .bs-container").remove();
    }, 0);
  }

  Shiny.addCustomMessageHandler("summaryClosePickers", function(message) {
    closeSummaryPickers(message && message.rowId ? message.rowId : null);
  });

  Shiny.addCustomMessageHandler("clearFileInput", function(message) {
    var inputId = message && message.id ? message.id : null;
    if (!inputId) {
      return;
    }

    var $fileInput = $("#" + inputId);
    if (!$fileInput.length) {
      return;
    }

    $fileInput.val("");
    $fileInput.trigger("change");

    var $container = $fileInput.closest(".shiny-input-container");
    $container.find("input[type='text']").val("");
    $container.find(".btn-file input[type='file']").val("");

    if (typeof Shiny.setInputValue === "function") {
      Shiny.setInputValue(inputId, null, { priority: "event" });
    }
  });

  Shiny.addCustomMessageHandler("clearModalBackdrop", function() {
    $(".modal-backdrop").remove();
    $("body").removeClass("modal-open").css("padding-right", "");
  });
})();
