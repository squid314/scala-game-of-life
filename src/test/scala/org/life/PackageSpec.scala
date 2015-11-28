package org.life

import org.scalatest.FlatSpec

class PackageSpec extends FlatSpec {
    val none = List( )
    val zeroZero = List( (0, 0) )

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
        assert( horiz( zeroZero ) == List( (0, 1) ) )
        assert( horiz( square ) == List( (0, 1), (0, 2), (1, 1), (1, 2) ) )
        assert( horiz( blinker ) == List( (0, 1), (0, 2), (0, 3) ) )
    }

    it should "move positions vertically" in {
        val horiz = offsetBy( 1, 0 ) _
        assert( horiz( none ) == none )
        assert( horiz( zeroZero ) == List( (1, 0) ) )
        assert( horiz( square ) == List( (1, 0), (1, 1), (2, 0), (2, 1) ) )
        assert( horiz( blinker ) == List( (1, 0), (1, 1), (1, 2) ) )
    }

    behavior of "flipHorizontally"

    it should "not create positions" in {
        assert( flipHorizontally( none ) == none )
        assert( flipHorizontally( zeroZero ) == zeroZero )
    }

//    it should ""
}
