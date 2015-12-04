package org.life

import org.scalatest.FlatSpec

class PackageSpec extends FlatSpec {
    val none: Positions = Set( )
    val zeroZero: Positions = Set( (0, 0) )
    val birdFlippedHorizontally: Positions = Set( (0, 0), (0, 1), (1, 0), (1, 1), (2, 1), (2, 2), (2, 3), (2, 4),
        (2, 5), (2, 6), (3, 2), (3, 5), (3, 6), (4, 2), (4, 6), (5, 2), (5, 3), (5, 4), (5, 5), (5, 6), (6, 4) )
    val birdFlippedVertically: Positions = Set( (0, 2), (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (2, 0), (2, 4), (3, 0),
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
        assert( horiz( zeroZero ) == Set( (0, 1) ) )
        assert( horiz( square ) == Set( (0, 1), (0, 2), (1, 1), (1, 2) ) )
        assert( horiz( blinker ) == Set( (0, 1), (0, 2), (0, 3) ) )
    }
    it should "move positions vertically" in {
        val horiz = offsetBy( 1, 0 ) _
        assert( horiz( none ) == none )
        assert( horiz( zeroZero ) == Set( (1, 0) ) )
        assert( horiz( square ) == Set( (1, 0), (1, 1), (2, 0), (2, 1) ) )
        assert( horiz( blinker ) == Set( (1, 0), (1, 1), (1, 2) ) )
    }

    behavior of "flipHorizontally"
    it should "not create positions" in {
        assert( flipHorizontally( none ) == none )
        assert( flipHorizontally( zeroZero ) == zeroZero )
        assert( flipHorizontally( square ) == square )
        assert( flipHorizontally( blinker ) == blinker )
    }
    it should "be able to flip a single position to the edge" in {
        assert( flipHorizontally( Set( (1, 1) ) ) == Set( (1, 0) ) )
    }
    it should "be able to flip the arrow" in {
        assert( flipHorizontally( arrow ) == Set( (0, 1), (1, 0), (2, 1) ) )
    }
    it should "be able to flip the bird" in {
        assert( flipHorizontally( bird ) == birdFlippedHorizontally )
    }

    behavior of "flipVertically"
    it should "not create positions" in {
        assert( flipVertically( none ) == none )
        assert( flipVertically( zeroZero ) == zeroZero )
        assert( flipVertically( square ) == square )
        assert( flipVertically( blinker ) == blinker )
    }
    it should "be able to flip a single position to the edge" in {
        assert( flipVertically( Set( (1, 1) ) ) == Set( (0, 1) ) )
    }
    it should "be able to flip the cross" in {
        assert( flipVertically( cross ) == Set( (0, 1), (1, 1), (2, 0), (2, 1), (2, 2), (3, 1) ) )
    }
    it should "be able to flip the bird" in {
        assert( flipVertically( bird ) == birdFlippedVertically )
    }
}
