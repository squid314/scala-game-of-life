package org.life

import org.scalatest.FlatSpec

class PackageSpec extends FlatSpec {
    val none: Positions = Positions( )
    val zeroZero: Positions = Positions( (0, 0) )
    val birdFlippedHorizontally: Positions = Positions( (0, 0), (0, 1), (1, 0), (1, 1), (2, 1), (2, 2), (2, 3), (2, 4),
        (2, 5), (2, 6), (3, 2), (3, 5), (3, 6), (4, 2), (4, 6), (5, 2), (5, 3), (5, 4), (5, 5), (5, 6), (6, 4) )
    val birdFlippedVertically: Positions = Positions( (0, 2), (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (2, 0), (2, 4), (3, 0),
        (3, 1), (3, 4), (4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (5, 5), (5, 6), (6, 5), (6, 6) )

    behavior of "offsetBy"
    it should "not create positions which didn't exist before" in {
        assert( offsetBy( 0, 0 )( none ) == none )
    }
    it should "leave positions where they are when given no offset" in {
        val noOffset = offsetBy( 0, 0 ) _
        assert( noOffset( none ) == none )
        assert( noOffset( zeroZero ) == zeroZero )
        assert( noOffset( square ) == square )
        assert( noOffset( blinker ) == blinker )
    }
    it should "move positions horizontally" in {
        val horiz = offsetBy( 0, 1 ) _
        assert( horiz( none ) == none )
        assert( horiz( zeroZero ) == Positions( (0, 1) ) )
        assert( horiz( square ) == Positions( (0, 1), (0, 2), (1, 1), (1, 2) ) )
        assert( horiz( blinker ) == Positions( (0, 1), (0, 2), (0, 3) ) )
    }
    it should "move positions vertically" in {
        val horiz = offsetBy( 1, 0 ) _
        assert( horiz( none ) == none )
        assert( horiz( zeroZero ) == Positions( (1, 0) ) )
        assert( horiz( square ) == Positions( (1, 0), (1, 1), (2, 0), (2, 1) ) )
        assert( horiz( blinker ) == Positions( (1, 0), (1, 1), (1, 2) ) )
    }

    behavior of "maxs"
    it should "not find anything in an empty set" in {
        assert( maxs( none ) == None )
    }
    it should "find the single 0,0 position" in {
        assert( maxs( zeroZero ) == Some( (0, 0) ) )
    }
    it should "find the single 5,5 position" in {
        assert( maxs( Positions( (5, 5) ) ) == Some( (5, 5) ) )
    }
    it should "find the max of the common shapes" in {
        assert( maxs( square ) == Some( (1, 1) ) )
        assert( maxs( glider ) == Some( (2, 2) ) )
        assert( maxs( arrow ) == Some( (2, 1) ) )
        assert( maxs( cross ) == Some( (3, 2) ) )
    }

    behavior of "flipHorizontally"
    it should "not create positions" in {
        assert( flipHorizontally( none ) == none )
        assert( flipHorizontally( zeroZero ) == zeroZero )
        assert( flipHorizontally( square ).toSet == square.toSet )
        assert( flipHorizontally( blinker ).toSet == blinker.toSet )
    }
    it should "be able to flip a single position to the edge" in {
        assert( flipHorizontally( Positions( (1, 1) ) ) == Positions( (1, 0) ) )
    }
    it should "be able to flip the arrow" in {
        assert( flipHorizontally( arrow ).toSet == Positions( (0, 1), (1, 0), (2, 1) ).toSet )
    }
    it should "be able to flip the bird" in {
        assert( flipHorizontally( bird ).toSet == birdFlippedHorizontally.toSet )
    }

    behavior of "flipVertically"
    it should "not create positions" in {
        assert( flipVertically( none ) == none )
        assert( flipVertically( zeroZero ) == zeroZero )
        assert( flipVertically( square ).toSet == square.toSet )
        assert( flipVertically( blinker ).toSet == blinker.toSet )
    }
    it should "be able to flip a single position to the edge" in {
        assert( flipVertically( Positions( (1, 1) ) ) == Positions( (0, 1) ) )
    }
    it should "be able to flip the cross" in {
        assert( flipVertically( cross ).toSet == Positions( (0, 1), (1, 1), (2, 0), (2, 1), (2, 2), (3, 1) ).toSet )
    }
    it should "be able to flip the bird" in {
        assert( flipVertically( bird ).toSet == birdFlippedVertically.toSet )
    }

    behavior of "rotate"
    it should "not create positions" in {
        assert( rotate( 0 )( none ) == none )
        assert( rotate( 0 )( zeroZero ) == zeroZero )
        assert( rotate( 0 )( square ) == square )
        assert( rotate( 0 )( blinker ) == blinker )
        assert( rotate( 0 )( arrow ) == arrow )
        assert( rotate( 0 )( cross ) == cross )
        assert( rotate( 0 )( bird ) == bird )
    }
    it should "safely rotate objects which are rotationally identical" in {
        assert( rotate( 0 )( none ) == none )
        assert( rotate( 1 )( none ) == none )
        assert( rotate( 2 )( none ) == none )
        assert( rotate( 3 )( none ) == none )
        assert( rotate( 0 )( square ).toSet == square.toSet )
        assert( rotate( 1 )( square ).toSet == square.toSet )
        assert( rotate( 2 )( square ).toSet == square.toSet )
        assert( rotate( 3 )( square ).toSet == square.toSet )

        assert( rotate( 0 )( blinker ).toSet == blinker.toSet )
        assert( rotate( 2 )( blinker ).toSet == blinker.toSet )
    }
    it should "rotate the arrow clockwise" in {
        assert( rotate( 1 )( arrow ).toSet == Positions( (0, 0), (0, 2), (1, 1) ).toSet )
    }
    it should "rotate the cross clockwise" in {
        assert( rotate( 1 )( cross ).toSet == Positions( (0, 2), (1, 0), (1, 1), (1, 2), (1, 3), (2, 2) ).toSet )
    }
    it should "rotate the arrow counterclockwise" in {
        assert( rotate( 3 )( arrow ).toSet == Positions( (1, 0), (1, 2), (0, 1) ).toSet )
    }
    it should "rotate the cross counterclockwise" in {
        assert( rotate( 3 )( cross ).toSet == Positions( (0, 1), (1, 0), (1, 1), (1, 2), (1, 3), (2, 1) ).toSet )
    }
    it should "rotate the arrow 180 degrees" in {
        assert( rotate( 2 )( arrow ).toSet == Positions( (2, 1), (1, 0), (0, 1) ).toSet )
    }
    it should "rotate the cross 180 degrees" in {
        assert( rotate( 2 )( cross ).toSet == Positions( (0, 1), (1, 1), (2, 0), (2, 1), (2, 2), (3, 1) ).toSet )
    }
}
