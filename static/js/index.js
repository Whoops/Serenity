function AppController($scope) {
    $scope.mode = "artists"
    $scope.template = function () {
        return "views/" + $scope.mode + ".html";
    }

    $scope.setArtist = function (artist) {
        $scope.artist = artist;
        $scope.album = null;
        $scope.mode = "albums";
    }

    $scope.setAlbum = function (album) {
        $scope.album = album;
        $scope.mode = "tracks";
    }

    $scope.clickArtists = function () {
        $scope.artist = null;
        $scope.album = null;
        $scope.mode = "artists";
    };
    
    $scope.clickAlbums = function () {
        $scope.artist = null;
        $scope.album = null;
        $scope.mode = "albums";
    }
}

function AlbumController($scope, $http) {
    $scope.albums = [];

    var url = '/albums';
    if ($scope.artist)
        url = '/artists/' + $scope.artist.id + '/albums';
    
    $http.get(url).success(function (data) {
        $scope.albums = data.sort(function (a, b) { return a.name.localeCompare(b.name); });
    });
}

function ArtistController($scope, $http) {
    $scope.artists = [];
    $http.get('/artists').success(function (data) {
        $scope.artists = data.sort(function (a, b) { return a.name.localeCompare(b.name); });
    });
}

function TrackController($scope, $rootScope, $http)
{
    $scope.tracks = [];

    var url = '/tracks';
    if ($scope.album)
        url = '/albums/' + $scope.album.id + '/tracks';
    $http.get(url).success(function (data) {
        $scope.tracks = data;
    });

    $scope.queue = function (track) {
        $rootScope.$emit('enqueue', track);
    }
}

function PlayingController($scope, $rootScope) {
    $scope.tracks = [];
    $scope.current = 0;
    $scope.playing = function () {
        if ($scope.current < $scope.tracks.length)
            return $scope.tracks[$scope.current];
        else
            return null;
    }

    $rootScope.$on('enqueue', function (event, track) {
        track.playing = function () {
            if (track === $scope.playing()) {
                return ["playing"];
            }
        }
        $scope.tracks.push(track);
        event.stopPropagation();
    });

    $scope.$watch('playing()', function (track) {
        if(track) {
            $("#player").jPlayer("setMedia", { mp3:  "/tracks/" + track.id });
            $("#player").jPlayer("play");
        }
    });

    $("#player").bind($.jPlayer.event.ended, function () {
        $scope.current += 1;
        $scope.$digest();
    });
}

$(function () {
    $("#player").jPlayer({ swfPath: "js/jquery/" });
});

/*function ContentViewModel () {
    var self = this;
    self.tracks = new TrackList ();
}

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

    
    self.playing = ko.computed(function () {
        if (self.current() < self.tracks().length)
            return self.tracks()[self.current()];
        else
            return null;
    });
    
    self.currentTitle = ko.computed( function () {
        var tr = self.playing()
        if (tr)
            return tr.title;
        else
            return "nothing";
    });
    
    
    self.enqueue = function (track) {
        track.selected = ko.computed(function () {
            return track === self.playing();
        });
        self.tracks.push(track);
    }
    
    self.advance = function () {
        self.current(self.current() + 1);
    }
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
    //var allTracks = new TrackList();
    var content = new ContentViewModel();
    var allTracks = content.tracks;
    
    $("#player").jPlayer({ swfPath: "js/jquery/" });
    
    ko.applyBindings(content, $("#content")[0]);
    ko.applyBindings(nowPlaying, $("#playing")[0]);

    nowPlaying.playing.subscribe(playTrack());
    $("#player").bind($.jPlayer.event.ended, nowPlaying.advance);

    $.getJSON('/tracks').success(function (data) {
        allTracks.tracks(data);
    });
}); */
