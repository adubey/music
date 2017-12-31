TROVE_JAR=/Users/adubey/src/music/lib/trove-2.0.2.jar
MALLET_JAR=/Users/adubey/src/music/lib/mallet.jar
MUSIC_JAR=/Users/adubey/src/music/target/scala-2.11/music_2.11-1.0.jar
JARS="$TROVE_JAR":"$MALLET_JAR":"$MUSIC_JAR"
JAVA_OPTS="-Xmx3G -Xms3G" ~/Downloads/scala-2.11.8/bin/scala -cp "$JARS" ca.dubey.music.percussion.Train $@
