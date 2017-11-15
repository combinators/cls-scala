require(['bootstrap'], function(bootstrap) {
    $(function() {
        var states = {};
        var otherButton = function (button) {
            if (button == "raw") { return "git"; }
            else { return "raw"; }
        }
        var toggle = function (i, path, button) {
            if (!(i in states) || states[i] != button) {
                $("#compute_text_" + i).addClass("hidden");
                $("#compute_spinner_" + i).removeClass("hidden");
                $("#compute_link_" + i).attr("href", "javascript:void(0)");
                $.ajax({
                    url : (path.endsWith("/") ? path : path + "/") +  "prepare?number=" + i,
                    dataType: "html"
                }).done(
                    function(rawInhabitant) {


                        if (button == "git") {
                            var paths = path.split('/');
                            var variationgit = paths[paths.length - 1];
                            var fullPath = $(location).attr("protocol") + "//" + $(location).attr("host") + path + "/" + variationgit + ".git"
                            var text =
                                "# clone into new git:\n" +
                                "git clone -b variation_" + i + " " + fullPath + "\n" +
                                "# checkout branch in existing git:\n" +
                                "git fetch origin\n" +
                                "git checkout -b variation_" + i + " origin/variation_" + i + "\n";
                            $("#solution_" + i).html(text);
                        } else {
                            $("#solution_" + i).html(rawInhabitant);
                        }



                        $("#solution_" + i).show("hidden");

                        $("#compute_button_" + i).addClass("hidden");

                        $("#" + button + "_button_" + i).removeClass("hidden");
                        $("#" + otherButton(button) + "_button_" + i).removeClass("hidden");
                        $("#" + button + "_button_" + i).addClass("active");
                        $("#" + otherButton(button) + "_button_" + i).removeClass("active");

                        states[i] = button;
                    }
                );
            } else {
                $("#solution_" + i).hide();
                $("#" + button + "_button_" + i).removeClass("active");
                delete states[i];
            }
        }
        toggleRaw = function (i, path) { toggle(i, path, "raw"); };
        toggleGit = function (i, path) { toggle(i, path, "git"); };
        toggleCompute = toggleRaw;
    });
});