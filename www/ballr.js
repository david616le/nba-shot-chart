$(function() {
  $(document).on("click", ".download-shot-chart", function(e) {
    e.preventDefault();
    var filename = $(this).data("filename");

    html2canvas($(".shot-chart-container"), {
      onrendered: function (canvas) {
        var a = document.createElement("a");
        a.href = canvas.toDataURL("image/png").replace("image/png", "image/octet-stream");
        a.download = filename;
        a.click();
      }
    });
  });

  $("#player_name").selectize({
    selectOnTab: true,
    maxOptions: 5000,
    onDropdownOpen: function() {
      this.clear('silent');
    }
  });

  setInterval(function() {
    $("#player_name_2").selectize({
      selectOnTab: true,
      maxOptions: 5000,
      onDropdownOpen: function() {
        this.clear('silent');
      }
    });

    $("#player_name_3").selectize({
      selectOnTab: true,
      maxOptions: 5000,
      onDropdownOpen: function() {
        this.clear('silent');
      }
    });

    $("#player_name_4").selectize({
      selectOnTab: true,
      maxOptions: 5000,
      onDropdownOpen: function() {
        this.clear('silent');
      }
    });
  }, 1000);


  $("#shot_zone_basic_filter").attr("title", "Select Areas");
  $("#shot_zone_angle_filter").attr("title", "Select Zones");
  $("#shot_distance_filter").attr("title", "Select Distances");

  $("#shot_zone_basic_filter, #shot_zone_angle_filter, #shot_distance_filter").selectpicker();
});
