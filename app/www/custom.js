(function() {
  if (typeof Shiny === "undefined" || typeof $ === "undefined") {
    return;
  }

  function getWorkspaceModalBody($source) {
    return $source.closest(".modal-dialog.workspace-modal-dialog").find(".modal-body").first();
  }

  function refreshMetadataAccordionLayout(containerId, onComplete) {
    var $containers = containerId ? $("#" + containerId) : $(".metadata-accordion-content");
    if (!$containers.length) {
      if (typeof onComplete === "function") {
        onComplete();
      }
      return;
    }

    window.requestAnimationFrame(function() {
      window.requestAnimationFrame(function() {
        $containers.each(function() {
          var $container = $(this);
          var $collapse = $container.closest(".accordion-collapse");

          $container.css({
            height: "auto",
            minHeight: "0"
          });

          if ($collapse.length) {
            $collapse.css("height", "auto");
          }

          if ($.fn.dataTable && $.fn.dataTable.isDataTable) {
            $container.find("table.dataTable").each(function() {
              if ($.fn.dataTable.isDataTable(this)) {
                $(this).DataTable().columns.adjust();
              }
            });
          }
        });

        $(window).trigger("resize");

        if (typeof onComplete === "function") {
          onComplete();
        }
      });
    });
  }

  function phase2OutputHasPlotContent(outputId) {
    var $output = outputId ? $("#" + outputId) : $();
    if (!$output.length) {
      return false;
    }

    return $output.find("img, canvas, svg").length > 0;
  }

  function phase2OutputHasTableContent(outputId) {
    var $output = outputId ? $("#" + outputId) : $();
    if (!$output.length) {
      return false;
    }

    if ($output.find(".dataTables_wrapper").length > 0) {
      return true;
    }

    if ($output.find("table tbody tr").length > 0) {
      return true;
    }

    return $output.find("table").length > 0;
  }

  function phase2ResultsAreReady(message) {
    var $root = message && message.rootId ? $("#" + message.rootId) : $();
    if (!$root.length) {
      return false;
    }

    if (!phase2OutputHasPlotContent(message.heatmapId)) {
      return false;
    }

    if (message.expectRanking && !phase2OutputHasTableContent(message.rankingTableId)) {
      return false;
    }

    if (message.expectTier && (!message.tierSectionId || $("#" + message.tierSectionId).length === 0)) {
      return false;
    }

    if (message.expectHighlights &&
        (!message.highlightsSectionId || $("#" + message.highlightsSectionId).length === 0)) {
      return false;
    }

    return true;
  }

  function clearWorkspaceModalBackdrop(force) {
    window.setTimeout(function() {
      if (!force && $(".modal.show").length > 0) {
        return;
      }

      $(".modal-backdrop").remove();
      $("body").removeClass("modal-open").css("padding-right", "");
    }, 0);
  }

  function bindWorkspaceModalContent(modalElement) {
    if (typeof Shiny === "undefined" || typeof Shiny.bindAll !== "function" || !document.body) {
      return;
    }

    Shiny.bindAll(document.body);
  }

  function signalWorkspaceModalReady(modalElement) {
    bindWorkspaceModalContent(modalElement);

    if (typeof Shiny.setInputValue !== "function") {
      return;
    }

    Shiny.setInputValue("workspace_modal_client_ready", Date.now(), {
      priority: "event"
    });
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

  Shiny.addCustomMessageHandler("clearModalBackdrop", function(message) {
    clearWorkspaceModalBackdrop(true);
  });

  Shiny.addCustomMessageHandler("refreshMetadataAccordion", function(message) {
    refreshMetadataAccordionLayout(
      message && message.id ? message.id : null,
      function() {
        if (typeof Shiny.setInputValue === "function" &&
            message &&
            message.readyInputId) {
          Shiny.setInputValue(
            message.readyInputId,
            message.requestId || null,
            { priority: "event" }
          );
        }
      }
    );
  });

  Shiny.addCustomMessageHandler("watchPhase2ResultsReady", function(message) {
    if (typeof Shiny.setInputValue !== "function" ||
        !message ||
        !message.readyInputId) {
      return;
    }

    var timeoutMs = message.timeoutMs || 10000;
    var startedAt = Date.now();
    var signaled = false;

    function signalReady() {
      if (signaled) {
        return;
      }

      signaled = true;
      Shiny.setInputValue(
        message.readyInputId,
        message.requestId || null,
        { priority: "event" }
      );
    }

    function pollUntilReady() {
      if (phase2ResultsAreReady(message) || (Date.now() - startedAt) >= timeoutMs) {
        signalReady();
        return;
      }

      window.requestAnimationFrame(function() {
        window.setTimeout(pollUntilReady, 100);
      });
    }

    window.requestAnimationFrame(function() {
      window.requestAnimationFrame(pollUntilReady);
    });
  });

  Shiny.addCustomMessageHandler("bindWorkspaceModalContent", function(message) {
    window.requestAnimationFrame(function() {
      window.requestAnimationFrame(function() {
        signalWorkspaceModalReady(document.body);
      });
    });
  });

  $(document).on("show.bs.select", ".modal-dialog.workspace-modal-dialog select", function() {
    var $modalBody = getWorkspaceModalBody($(this));
    if (!$modalBody.length) {
      return;
    }

    $modalBody.data("workspacePickerScrollTop", $modalBody.scrollTop());
  });

  $(document).on("shown.bs.select", ".modal-dialog.workspace-modal-dialog select", function() {
    var $modalBody = getWorkspaceModalBody($(this));
    var scrollTop = $modalBody.data("workspacePickerScrollTop");
    if (!$modalBody.length || typeof scrollTop !== "number") {
      return;
    }

    window.requestAnimationFrame(function() {
      $modalBody.scrollTop(scrollTop);
    });
  });

  $(document).on("shown.bs.tab", ".metadata-editor-shell [data-bs-toggle='tab'], .metadata-editor-shell [data-bs-toggle='pill']", function() {
    var $container = $(this).closest(".metadata-accordion-content");
    refreshMetadataAccordionLayout($container.attr("id") || null);
  });

  $(document).on("shown.bs.collapse", ".accordion .accordion-collapse", function() {
    var $container = $(this).find(".metadata-accordion-content").first();
    if ($container.length) {
      refreshMetadataAccordionLayout($container.attr("id") || null);
    }
  });

  $(document).on("shown.bs.modal", "#shiny-modal", function() {
    var modalElement = this;

    window.requestAnimationFrame(function() {
      window.requestAnimationFrame(function() {
        signalWorkspaceModalReady(modalElement);
      });
    });
  });

  $(document).on("hidden.bs.modal", ".workspace-modal-dialog", function() {
    clearWorkspaceModalBackdrop(false);
  });

})();
