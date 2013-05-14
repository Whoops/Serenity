// Presumed not to work in IE < 9

function AppController($scope, $rootScope) {
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

    $scope.clickTracks = function () {
        $scope.artist = null;
        $scope.album = null;
        $scope.mode = "tracks";
    }

    $scope.playNext = function () {
        $rootScope.$emit("playNext");
    }

    $scope.playLast = function () {
        $rootScope.$emit("playLast");
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
        $scope.tracks = data.sort(function (a, b) {
            if ($scope.album)
                return a.track - b.track;
            else
                return a.title.localeCompare(b.title);
        });
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

    $scope.playTrack = function(track) {
        $scope.current = $scope.tracks.indexOf(track);
    }

    $rootScope.$on('enqueue', function (event, track) {
        var newTrack = angular.copy(track);
        newTrack.playing = function () {
            if (newTrack === $scope.playing()) {
                return ["playing"];
            }
        }
        $scope.tracks.push(newTrack);
        event.stopPropagation();
    });

    $rootScope.$on("playNext", function (event) {
        if ($scope.current < $scope.tracks.length - 1)
            $scope.current += 1;
        event.stopPropagation();
    });

    $rootScope.$on("playLast", function (event) {
        if ($scope.current > 0)
            $scope.current -= 1;
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
