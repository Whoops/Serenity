function TrackList () {
    var self = this;
    self.tracks = ko.observableArray();
    self.queueTrack = function (track) {
        nowPlaying.enqueue(track);
    }
}

function Playlist () {
    var self = this;
    self.tracks = ko.observableArray();
    self.current = ko.observable(0);
    self.enqueue = function (track) {
        self.tracks.push(track);
    }
    self.playing = ko.computed(function () {
        if (self.current() < self.tracks().length)
            return self.tracks()[self.current()];
        else
            return null;
    });
    self.advance = function () {
        self.current(self.current() + 1);
    }
    self.currentTitle = ko.computed( function () {
        var tr = self.playing()
        if (tr)
            return tr.title;
        else
            return "nothing";
    });
}
var nowPlaying = new Playlist();

// closure to keep track of the currently playing track
// and prevent the current track restarting if track is the
// same as current
function playTrack () {
    var tr = null;
    var fn = function (track) {
        if (tr !== track && track) {
            $("#player").jPlayer("setMedia", { mp3:  "/tracks/" + track.id });
            $("#player").jPlayer("play");
            tr = track;
        }
    }
    return fn;
}

$(function () {
    var allTracks = new TrackList();
    $("#player").jPlayer({ swfPath: "js/jquery/" });
    
    ko.applyBindings(allTracks, $("#content")[0]);
    ko.applyBindings(nowPlaying, $("#playing")[0]);

    nowPlaying.playing.subscribe(playTrack());
    $("#player").bind($.jPlayer.event.ended, nowPlaying.advance);

    $.getJSON('/tracks').success(function (data) {
        allTracks.tracks(data);
    });
});



/*var templates = {}

function registerTemplate (name) {
    var reg = function (text) {
        templates[name] = Mustache.compile(text);
    }
    return reg;
}

function renderContent (type) {
    var template = templates[type];
    var rend = function (data) {
        var view = {}
        view[type] = data;
        $("#content").html(template(view));
    }
    return rend;
}

function registerTemplates () {
    $.get("templates/artists.html").success(registerTemplate("artists"));
    $.get("templates/tracks.html").success(registerTemplate("tracks"));
}

function displayArtists () {
    $.getJSON("artists").success(renderContent("artists"));
}

function displayTracks(artist) {
    $.getJSON("artists/" + artist + "/tracks").success(renderContent("tracks"));
}

function playTrack(track) {
    $("#player").jPlayer("setMedia", { mp3:  "/tracks/" + track.attr("id") });
    $("#player").jPlayer("play");
}

$(function () {
    $("#player").jPlayer({ swfPath: "js/jquery/" });
    registerTemplates();
    $("#side-artists").click(function () {
        displayArtists();
    });
    
    $("#content").on("click", ".artist", function () {
        displayTracks($(this).attr("id"));
    });
    $("#content").on("click", ".track", function () {
        playTrack($(this));
    });
});
*/
